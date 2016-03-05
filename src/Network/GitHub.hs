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
    , organisationTeams
    , getTeam
    , teamMembers
    , teamRepositories
    , user
    , userRepositories
    -- * GitHub monad
    -- $github
    , GitHub
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

import Data.Proxy

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
userRepositories :: GitHub [Repository]
userRepositories = github (Proxy :: Proxy UserRepositories)

-- $github
--
-- Use the 'runGitHub' function to execute the 'GitHub' client function.


-- $pagination
--
-- Functions for managing the pagination features of the GitHub API
--
