{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.GitHub.Authentication 
where

import Data.Monoid
import Data.String
import Data.Text

import Servant.API

newtype Token = Token Text deriving (Eq)
instance IsString Token where
    fromString s = Token (fromString s)
instance ToText Token where
    toText (Token t) = "token " <> t

type OAuth2Token = Header "Authorization" Token
