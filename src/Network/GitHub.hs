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

import Control.Monad.Trans.Reader
import Data.Proxy
import Data.Text

import Servant.API
import Servant.Client

import Network.GitHub.API
import Network.GitHub.Types
import Network.GitHub.Authentication
import Network.GitHub.Client

orgTeams :: OrgLogin -> GitHub [Team]
orgTeams = ReaderT . flip call  
  where
    layout :: Proxy (Header "User-Agent" Text :> OAuth2Token :> OrgTeams)
    layout = Proxy
    call :: Maybe AuthToken -> Client OrgTeams
    call = client layout host (Just useragent)
 
getOrgs :: GitHub [Organisation]
getOrgs = github "servant-github" (Proxy :: Proxy UserOrgs)

--orgTeams' :: OrgLogin -> GitHub [Team]
--orgTeams' = github (Proxy :: Proxy OrgTeams)
