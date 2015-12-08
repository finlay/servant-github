{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.GitHub.Authentication 
where

import Data.Monoid
import Data.String
import Data.Text

import Servant.API

newtype AuthToken = AuthToken Text deriving (Eq)
instance IsString AuthToken where
    fromString s = AuthToken (fromString s)
instance ToText AuthToken where
    toText (AuthToken t) = "token " <> t

type OAuth2Token = Header "Authorization" AuthToken
