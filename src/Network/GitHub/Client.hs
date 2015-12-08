{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Network.GitHub.Client
where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Either
import Data.Proxy
import Data.Text

import Servant.API
import Servant.Client

import Network.GitHub.Authentication

host :: BaseUrl
host = BaseUrl Https "api.github.com" 443

useragent :: Maybe Text
useragent = Just "servant-github"

type GitHub = ReaderT (Maybe AuthToken) (EitherT ServantError IO) 

runGitHub :: GitHub a -> Maybe AuthToken -> IO (Either ServantError a)
runGitHub comp = runEitherT . runReaderT comp 

class HasGitHub layout a where
    type GH layout a :: *
    github :: Proxy layout -> GH layout a
 
instance (HasClient layout, Client layout ~ EitherT ServantError IO a)
    => HasGitHub layout a where
    type GH layout a = GitHub a
    github Proxy = do
        token <- ask
        lift $ call useragent token
      where
        layout :: Proxy (Header "User-Agent" Text :> OAuth2Token :> layout)
        layout = Proxy
        call :: Maybe Text -> Maybe AuthToken -> Client layout
        call = client layout host



--orgTeams :: OrgLogin -> GitHub [Team]
--orgTeams org = do
--    token <- ask
--    lift $ call useragent token org
--  where
--    layout :: Proxy (Header "User-Agent" Text :> OAuth2Token :> OrgTeams)
--    layout = Proxy
--    call :: Maybe Text -> Maybe AuthToken -> Client OrgTeams
--    call = client layout host
 


