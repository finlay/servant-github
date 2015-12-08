{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.GitHub 
    ( AuthToken(..)
    , module Network.GitHub
    , module Network.GitHub.Types
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

host :: BaseUrl
host = BaseUrl Https "api.github.com" 443

useragent :: Maybe Text
useragent = Just "servant-github"

type GitHub = ReaderT (Maybe AuthToken) (EitherT ServantError IO) 

runGitHub :: GitHub a -> Maybe AuthToken -> IO (Either ServantError a)
runGitHub comp token = runEitherT $ runReaderT comp token

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
getOrgs = do
    token <- ask    
    lift $ call useragent token
  where 
    layout :: Proxy (Header "User-Agent" Text :> OAuth2Token :> UserOrgs)
    layout = Proxy
    call :: Maybe Text -> Maybe AuthToken -> Client UserOrgs
    call = client layout host
    

