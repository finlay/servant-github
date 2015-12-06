{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.GitHub 
where

--import Control.Applicative
--import Control.Monad
--import Control.Monad.IO.Class
import Control.Monad.Trans.Either
--import Data.Aeson
import Data.Monoid
import Data.Proxy
import Data.Text
import Data.String
--import GHC.Generics
import Servant.API
import Servant.Client

import Network.GitHub.Organisation

newtype Token = Token Text deriving (Eq)
instance IsString Token where
    fromString s = Token (fromString s)
instance ToText Token where
    toText (Token t) = "token " <> t
type OAuth2Token = Header "Authorization" Token
type UA = Header "User-Agent" Text

type UserOrgs = "user" :> "orgs" :> UA :> OAuth2Token :>Get '[JSON] [Organisation]

api :: Proxy UserOrgs
api = Proxy

getOrgs :: Maybe Text -> Maybe Token -> EitherT ServantError IO [Organisation]
getOrgs = client api (BaseUrl Https "api.github.com" 443)
