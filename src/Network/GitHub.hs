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

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Either
import Data.Proxy
import Data.Text

import Servant.API
import Servant.Client

import Network.GitHub.API
import Network.GitHub.Types
import Network.GitHub.Authentication
import Network.GitHub.Client

orgTeams :: OrgLogin -> GitHub [Team]
orgTeams org = do
    token <- ask
    lift $ call useragent token org
  where
    layout :: Proxy (Header "User-Agent" Text :> OAuth2Token :> OrgTeams)
    layout = Proxy
    call :: Maybe Text -> Maybe AuthToken -> Client OrgTeams
    call = client layout host
 
getOrgs :: GitHub [Organisation]
getOrgs = github (Proxy :: Proxy UserOrgs)

--orgTeams' :: OrgLogin -> GitHub [Team]
--orgTeams' = github (Proxy :: Proxy OrgTeams)
