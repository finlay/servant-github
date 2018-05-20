{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
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
    , userByLogin
    , userRepositories
    , userInstallationRepositories
    , organisationRepositories
    , installationRepositories
    , appInstallations
    , userInstallations
    , userTeams
    , repositoryCollaborators
    , getCommit
    , getContent
    , getIssues
    , integrationJWT
    , reqInstallationAccessToken
    , gists
    , editGist
    , createGist
    -- * GitHub monad
    -- $github
    , GitHub
    , runGitHubClientM
    , runGitHubNotApiClientM
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

import Control.Lens
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Except

import Data.Proxy
import Data.Monoid ((<>))
import Data.Time.Clock (getCurrentTime, UTCTime(..), addUTCTime)
import Data.Time.Clock.POSIX
       (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as B (toStrict)

import Servant.Client (client, ClientM)

import Crypto.Random (MonadRandom(..))
import Crypto.JOSE.JWK (JWK)
import qualified Crypto.JWT as JWT
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

-- | Lookup user by login
userByLogin :: Maybe String -> GitHub User
userByLogin = github (Proxy :: Proxy GetUserByLogin)

-- | Get repositories for the authorised user
userRepositories :: Maybe String -> GitHub [Repository]
userRepositories = github (Proxy :: Proxy UserRepositories)

-- | List repositories that are accessible to the authenticated user for an installation.
userInstallationRepositories :: Int -> GitHub Repositories
userInstallationRepositories = github (Proxy :: Proxy UserInstallationRepositories)

-- | Get repositories for an organisation login
organisationRepositories :: OrgLogin -> GitHub [Repository]
organisationRepositories = github (Proxy :: Proxy OrganisationRepositories)

-- | Get repositories for the installation (current token should be an installation token)
installationRepositories :: GitHub Repositories
installationRepositories = github (Proxy :: Proxy InstallationRepositories)

-- | Get installations for the appliction
appInstallations :: GitHub [Installation]
appInstallations = github (Proxy :: Proxy AppInstallations)

-- | List installations that are accessible to the authenticated user.
userInstallations :: Maybe String -> GitHub Installations
userInstallations = github (Proxy :: Proxy UserInstallations)

-- | List teams that are the user is a member of.
userTeams :: GitHub [Team]
userTeams = github (Proxy :: Proxy UserTeams)

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

integrationJWT
  :: (MonadRandom m, MonadError e m, JWT.AsError e)
  => JWK
  -> Int
  -> UTCTime
  -> m Text
integrationJWT key integrationId now' = do
    let now = posixSecondsToUTCTime . fromInteger . round $ utcTimeToPOSIXSeconds now'
        claimsSet = JWT.emptyClaimsSet
          & JWT.claimIss .~ Just (review JWT.string $ show integrationId)
          & JWT.claimIat .~ Just (JWT.NumericDate now)
          & JWT.claimExp .~ Just (JWT.NumericDate $ addUTCTime 60 now)
    signed <- JWT.signClaims key (newJWSHeader ((), RS256)) claimsSet
    return . T.decodeUtf8 . B.toStrict $ encodeCompact signed

reqInstallationAccessToken
  :: JWK -> Int -> Int -> Maybe InstallationUser -> ClientM InstallationAccessToken
reqInstallationAccessToken key integrationId installationId mbUser = do
    now <- liftIO getCurrentTime
    jwtEth <- liftIO . runExceptT $ integrationJWT key integrationId now
    case jwtEth of
      Left (err :: JWT.Error) -> liftIO . fail $ show err
      Right jwt ->
        client (Proxy :: Proxy ReqInstallationAccessToken)
              installationId
              (Just "Gorbachev IO")
              (Just $ "Bearer " <> T.unpack jwt)
              mbUser

---- Gist
-- | Get gists for the authorised user
gists :: Maybe UTCTime -> GitHub [Gist]
gists = github (Proxy :: Proxy Gists)

-- | Edit a gist
editGist :: GistId -> GistEdit -> GitHub Gist
editGist = github (Proxy :: Proxy EditGist)

-- | Create a gist
createGist :: GistCreate -> GitHub Gist
createGist = github (Proxy :: Proxy CreateGist)

-- $github
--
-- Use the 'runGitHub' function to execute the 'GitHub' client function.


-- $pagination
--
-- Functions for managing the pagination features of the GitHub API
--
