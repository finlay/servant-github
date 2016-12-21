{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
-- |
-- Module      : Network.GitHub
-- Copyright   : (c) Finlay Thompson, 2015
-- License     : BSD3
-- Maintainer  : finlay.thompson@gmail.com
-- Stability   : experimental
--
-- The GitHub monad provides support for:
--
--     - Managing the authentication token. It can be Nothing, in which case
--       no Authentication header is sent to the API,
--
--     - Setting the User-agent header string. This defaults to "servant-github",
--       but can be set inside the GitHub monad using the 'setUserAgent', and
--
--     - Keeping track of the pagination in the case of calls that return lists
--       of objects.

module Network.GitHub
    (
    -- * GitHub API calls
    -- $client
      userOrganisations
    , userOrganisationMemberships
    , organisationTeams
    , getTeam
    , teamMembers
    , teamRepositories
    , user
    , userRepositories
    , organisationRepositories
    , installationRepositories
    , repositoryCollaborators
    , getCommit
    , getContent
    , getIssues
    , integrationJWT
    , reqInstallationAccessToken
    -- * GitHub monad
    -- $github
    , GitHub
    , runGitHubClientM
    , runGitHub'
    , runGitHub
    , AuthToken
    , setUserAgent
    -- * Pagination
    -- $pagination
    , resetPagination
    , recurseOff
    , recurseOn
    , pageSize
    , getLinks
    , module Network.GitHub.API
    , module Network.GitHub.Types
    )
where

import Control.Monad.IO.Class (MonadIO(..))

import Data.Proxy
import Data.Monoid ((<>))
import Data.Time.Clock (getCurrentTime, UTCTime(..), addUTCTime)
import Data.Time.Clock.POSIX
       (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.Text (Text)
import qualified Data.Text as T (unpack, pack)
import qualified Data.Text.Encoding as T (decodeUtf8')
import qualified Data.ByteString.Lazy as B (toStrict)

import Servant.Client (client, ClientM)

import Crypto.Random (MonadRandom(..))
import Crypto.JOSE.JWK (JWK(..))
import Crypto.JWT (createJWSJWT, _claimIss, _claimIat, _claimExp)
import qualified Crypto.JWT as JWT
       (NumericDate(..), fromString, emptyClaimsSet)
import Crypto.JOSE.JWS (Alg(..), newJWSHeader)
import Crypto.JOSE.Compact (encodeCompact)

import Network.GitHub.API
import Network.GitHub.Types
import Network.GitHub.Client

-- $client
--
-- Functions that directly access the GitHub API. These functions all run
-- in the 'GitHub' monad.
--

-- | Get list of 'Organisation' records for authorised user
userOrganisations :: GitHub [Organisation]
userOrganisations = github (Proxy :: Proxy UserOrganisations)

-- | Get list of 'OrganisationMember' records for authorised user
userOrganisationMemberships :: GitHub [OrganisationMember]
userOrganisationMemberships = github (Proxy :: Proxy UserOrganisationMemberships)

-- | Get list of 'Team' records, given the organisation login
organisationTeams :: OrgLogin -> GitHub [Team]
organisationTeams = github (Proxy :: Proxy OrganisationTeams)

-- | Get the 'Team' record associated to a TeamId
getTeam :: TeamId -> GitHub Team
getTeam = github (Proxy :: Proxy GetTeam)

-- | Get list of 'Member' records assoctiated to 'Team' given by Team Id
teamMembers :: TeamId -> GitHub [Member]
teamMembers = github (Proxy :: Proxy TeamMembers)

-- | Get list of 'Repository' records assoctiated to 'Team' given by Team Id
teamRepositories :: TeamId -> GitHub [Repository]
teamRepositories = github (Proxy :: Proxy TeamRepositories)

-- | Get the current user for the authorised user
user :: GitHub User
user = github (Proxy :: Proxy GetUser)
--
-- | Get repositories for the authorised user
userRepositories :: Maybe String -> GitHub [Repository]
userRepositories = github (Proxy :: Proxy UserRepositories)

--
-- | Get repositories for an organisation login
organisationRepositories :: OrgLogin -> GitHub [Repository]
organisationRepositories = github (Proxy :: Proxy OrganisationRepositories)

--
-- | Get repositories for the installation (current token should be an installation token)
installationRepositories :: GitHub Repositories
installationRepositories = github (Proxy :: Proxy InstallationRepositories)

--
-- | Get repositories for the installation (current token should be an installation token)
repositoryCollaborators :: OrgLogin -> RepoName -> GitHub [Member]
repositoryCollaborators = github (Proxy :: Proxy RepositoryCollaborators)

-- | Get commit for repo and reference
getCommit :: OrgLogin -> RepoName -> Sha -> GitHub Commit
getCommit = github (Proxy :: Proxy GetCommit)

-- | Get content for repo and reference and path
getContent :: OrgLogin -> RepoName -> String -> Maybe String -> Maybe String -> GitHub Content
getContent = github (Proxy :: Proxy GetContent)

-- | Get issuers for a repository
type GHOptions =[(String, String)]
getIssues :: GHOptions -> Owner -> RepoName -> GitHub [Issue]
getIssues opts owner repo
  = github (Proxy :: Proxy GetIssues) owner repo
            (lookup "milestone" opts)
            (lookup "state" opts)
            (lookup "assignee" opts)
            (lookup "creator" opts)
            (lookup "mentioned" opts)
            (lookup "labels" opts)
            (lookup "sort" opts)
            (lookup "direction" opts)
            (lookup "since" opts)

integrationJWT :: MonadRandom m => JWK -> Int -> UTCTime -> m Text
integrationJWT key integrationId now' = do
    let now = posixSecondsToUTCTime . fromInteger . round $ utcTimeToPOSIXSeconds now'
        claimsSet = JWT.emptyClaimsSet
          { _claimIss = Just $ JWT.fromString (T.pack $ show integrationId)
          , _claimIat = Just $ JWT.NumericDate now
          , _claimExp = Just $ JWT.NumericDate (addUTCTime 60 now)
          }
    createJWSJWT key (newJWSHeader RS256) claimsSet >>= \case
        Left _err -> error "Unable to construct integration claims set"
        Right jwt -> case encodeCompact jwt of
            Left err -> error $ show err
            Right jwtBS -> case T.decodeUtf8' $ B.toStrict jwtBS of
                Left err -> error $ show err
                Right jwtText -> return jwtText

reqInstallationAccessToken :: JWK -> Int -> Int -> Maybe InstallationUser -> ClientM InstallationAccessToken
reqInstallationAccessToken key integrationId installationId mbUser = do
    now <- liftIO getCurrentTime
    jwt <- liftIO $ integrationJWT key integrationId now
    client (Proxy :: Proxy ReqInstallationAccessToken)
           installationId
           (Just "Gorbachev IO")
           (Just $ "Bearer " <> T.unpack jwt)
           mbUser


-- $github
--
-- Use the 'runGitHub' function to execute the 'GitHub' client function.


-- $pagination
--
-- Functions for managing the pagination features of the GitHub API
--
