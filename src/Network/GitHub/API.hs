{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module      : Network.GitHub.API
-- Copyright   : (c) Finlay Thompson, 2015
-- License     : BSD3
-- Maintainer  : finlay.thompson@gmail.com
-- Stability   : experimental
 
module Network.GitHub.API
where

import Servant.API

import Network.GitHub.Types

-- | <https://developer.github.com/v3/orgs/#list-your-organizations>
type UserOrganisations = "user" :> "orgs" :> Get '[JSON] [Organisation]

-- | <https://developer.github.com/v3/orgs/teams/#list-teams>
type OrganisationTeams = "orgs" :> Capture "org" OrgLogin :> "teams" :> Get '[JSON] [Team]

-- | <https://developer.github.com/v3/orgs/teams/#list-team-members>
type TeamMembers = "teams" :> Capture "id" TeamId :> "members" :> Get '[JSON] [Member]

-- | <https://developer.github.com/v3/orgs/teams/#list-team-repos>
type TeamRepositories = "teams" :> Capture "id" TeamId :> "repos" :> Get '[JSON] [Repository]

-- | <https://developer.github.com/v3/orgs/teams/#get-team>
type GetTeam = "teams" :> Capture "id" TeamId :> Get '[JSON] Team

-- | <https://developer.github.com/v3/users/#get-the-authenticated-user>
type GetUser = "user" :> Get '[JSON] User

-- | <https://developer.github.com/v3/repos/#list-your-repositories>
type UserRepositories = "user" :> "repos" :> QueryParam "type" String :> Get '[JSON] [Repository]
