{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.GitHub.API
where

import Control.Monad.Trans.Either
import Data.Proxy
import Data.Text
import Servant.API
import Servant.Client

import Network.GitHub.Authentication
import Network.GitHub.Types

type UserOrgs = OAuth2Token :> "user" :> "orgs" :> Get '[JSON] [Organisation]
type OrgTeams = OAuth2Token :> "orgs" :> Capture "org" OrgLogin :> "teams" :> Get '[JSON] [Team]





