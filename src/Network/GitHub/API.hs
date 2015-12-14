{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Network.GitHub.API
where

import Servant.API

import Network.GitHub.Types

type UserOrganisations = "user" :> "orgs" :> Get '[JSON] [Organisation]
type OrganisationTeams = "orgs" :> Capture "org" OrgLogin :> "teams" :> Get '[JSON] [Team]
type TeamMembers = "teams" :> Capture "id" TeamId :> "members" :> Get '[JSON] [Member]
type TeamRepositories = "teams" :> Capture "id" TeamId :> "repos" :> Get '[JSON] [Repository]


