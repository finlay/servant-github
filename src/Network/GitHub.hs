{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.GitHub 
    ( module Network.GitHub
    , module Network.GitHub.API
    , module Network.GitHub.Types
    , module Network.GitHub.Client
    )
where

import Data.Proxy

import Network.GitHub.API
import Network.GitHub.Types
import Network.GitHub.Client

useragent :: UserAgent
useragent = "servant-github"

userOrganisations :: GitHub [Organisation]
userOrganisations = github useragent (Proxy :: Proxy UserOrganisations)

organisationTeams :: OrgLogin -> GitHub [Team]
organisationTeams = github useragent (Proxy :: Proxy OrganisationTeams)

getTeam :: TeamId -> GitHub Team
getTeam = github useragent (Proxy :: Proxy GetTeam)

teamMembers :: TeamId -> GitHub [Member]
teamMembers = github useragent (Proxy :: Proxy TeamMembers)

teamRepositories :: TeamId -> GitHub [Repository]
teamRepositories = github useragent (Proxy :: Proxy TeamRepositories)
