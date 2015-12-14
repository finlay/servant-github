{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.GitHub 
    ( module Network.GitHub
    , module Network.GitHub.API
    , module Network.GitHub.Types
    , module Network.GitHub.Authentication
    , module Network.GitHub.Client
    )
where

import Data.Proxy

import Network.GitHub.API
import Network.GitHub.Types
import Network.GitHub.Authentication
import Network.GitHub.Client

getOrgs :: GitHub [Organisation]
getOrgs = github "servant-github" (Proxy :: Proxy UserOrgs)

orgTeams :: OrgLogin -> GitHub [Team]
orgTeams = github "servant-github" (Proxy :: Proxy OrgTeams)
