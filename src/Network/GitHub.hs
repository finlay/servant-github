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

userOrganisations :: GitHub [Organisation]
userOrganisations = github "servant-github" (Proxy :: Proxy UserOrganisations)

organisationTeams :: OrgLogin -> GitHub [Team]
organisationTeams = github "servant-github" (Proxy :: Proxy OrganisationTeams)

teamMembers :: TeamId -> GitHub [Member]
teamMembers = github "servant-github" (Proxy :: Proxy TeamMembers)

teamRepositories :: TeamId -> GitHub [Repository]
teamRepositories = github "servant-github" (Proxy :: Proxy TeamRepositories)
