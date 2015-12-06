{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.GitHub 
    ( module Network.GitHub.Authentication
    , module Network.GitHub
    )
where

--import Control.Applicative
--import Control.Monad
--import Control.Monad.IO.Class
import Control.Monad.Trans.Either
--import Data.Aeson
import Data.Proxy
import Data.Text
--import GHC.Generics
import Servant.API
import Servant.Client

import Network.GitHub.Authentication
import Network.GitHub.Organisation

type UserOrgs = OAuth2Token :> "user" :> "orgs" :> Get '[JSON] [Organisation]

type UserAgent = Text

type API = Header "User-Agent" UserAgent :> UserOrgs

api :: Proxy API
api = Proxy

getOrgs :: Maybe UserAgent -> Maybe Token -> EitherT ServantError IO [Organisation]
getOrgs = client api (BaseUrl Https "api.github.com" 443)
