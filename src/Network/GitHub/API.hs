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

-- | <https://developer.github.com/v3/orgs/members/#list-your-organization-memberships>
type UserOrganisationMemberships = "user" :> "memberships" :> "orgs" :> Get '[JSON] [OrganisationMember]

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

-- | <https://developer.github.com/v3/users/#get-a-single-user>
type GetUserByLogin = "user" :> QueryParam "username" String :> Get '[JSON] User

-- | <https://developer.github.com/v3/repos/#list-your-repositories>
type UserRepositories = "user" :> "repos" :> QueryParam "type" String :> Get '[JSON] [Repository]

-- | <https://developer.github.com/v3/apps/installations/#list-repositories-accessible-to-the-user-for-an-installation>
type UserInstallationRepositories = "user" :> "installations" :> Capture "installation_id" Int :> "repositories" :> Get '[EarlyAccessJSON] Repositories

-- | <https://developer.github.com/v3/repos/#list-organization-repositories>
type OrganisationRepositories = "orgs" :> Capture "org" OrgLogin :> "repos" :> Get '[JSON] [Repository]

-- | <https://developer.github.com/early-access/integrations/integrations-vs-oauth-applications/#repository-discovery>
type InstallationRepositories = "installation" :> "repositories" :> Get '[EarlyAccessJSON] Repositories

-- | <https://developer.github.com/v3/apps/#find-installations>
type AppInstallations = "app" :> "installations" :> Get '[EarlyAccessJSON] [Installation]

-- | <https://developer.github.com/v3/apps/#list-installations-for-user>
type UserInstallations = "user" :> "installations" :> QueryParam "access_token" String :> Get '[EarlyAccessJSON] Installations

-- | <https://developer.github.com/v3/repos/collaborators/#list-collaborators>
type RepositoryCollaborators = "repos" :> Capture "org" OrgLogin :> Capture "repo" RepoName :> "collaborators" :> Get '[JSON] [Member]

-- | <https://developer.github.com/v3/repos/commits/#get-a-single-commit>
type GetCommit
    = "repos" :> Capture "org" OrgLogin :> Capture "repo" RepoName
   :> "commits" :> Capture "sha" Sha :> Get '[JSON] Commit

-- | <https://developer.github.com/v3/repos/contents/#get-contents>
-- GET /repos/:owner/:repo/contents/:path
type GetContent
    = "repos"  :> Capture "org" OrgLogin :> Capture "repo" RepoName
   :> "contents" :>  Capture "path" String :> QueryParam "ref" String :>  QueryParam "path" String
   :> Get '[JSON] Content

-- | https://developer.github.com/v3/issues/#list-issues-for-a-repository
-- GET /repos/:owner/:repo/issues
type GetIssues
    = "repos" :> Capture "owner" Owner :> Capture "repo" RepoName :> "issues"
   :> QueryParam "milestone" String :> QueryParam "state" String
   :> QueryParam "assignee" String :> QueryParam "creator" String
   :> QueryParam "mentioned" String :> QueryParam "labels" String
   :> QueryParam "sort" String :> QueryParam "direction" String
   :> QueryParam "since" String
   :> Get '[JSON] [Issue]

type ReqInstallationAccessToken = "installations" :> Capture "installation_id" Int :> "access_tokens"
   :> Header "User-Agent" String
   :> Header "Authorization" String
   :> ReqBody '[JSON] (Maybe InstallationUser)
   :> Post '[EarlyAccessJSON] InstallationAccessToken
