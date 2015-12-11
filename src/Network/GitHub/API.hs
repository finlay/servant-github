{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Network.GitHub.API
where

import Servant.API

import Network.GitHub.Types

type UserOrgs = "user" :> "orgs" :> Get '[JSON] [Organisation]
type OrgTeams = "orgs" :> Capture "org" OrgLogin :> "teams" :> Get '[JSON] [Team]




