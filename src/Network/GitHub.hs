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

type GitHub = ReaderT (Maybe AuthToken) (EitherT ServantError IO) 

runGitHub :: Maybe AuthToken -> GitHub a -> IO (Either ServantError a)
runGitHub token comp = runEitherT $ runReaderT comp token

orgTeams :: OrgLogin -> GitHub [Team]
orgTeams org = do
    token <- ask
    lift $ call (Just "servant-github") token org
  where
    call :: Maybe Text -> Maybe AuthToken -> OrgLogin -> EitherT ServantError IO [Team]
    call = client (Proxy :: Proxy (Header "User-Agent" Text :> OrgTeams)) host



getOrgs :: GitHub [Organisation]
getOrgs = do
    token <- ask    
    lift $ call (Just "servant-github") token
  where 
    call :: Maybe Text -> Maybe AuthToken -> EitherT ServantError IO [Organisation]
    call = client (Proxy :: Proxy (Header "User-Agent" Text :> UserOrgs)) host
    



-- | Monad for running the client code
--runGitHub :: 

