{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.GitHub.Types.Gist
  (
    GistId
  , Gist(..)
  , GistOwner(..)
  , File(..)
  ) where

import Control.Applicative
import Data.Aeson
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Time.Clock
import GHC.Generics

import Network.GitHub.Types.Core

--- Gist Types
type GistId = Text
data Gist = Gist
  { url          :: Text
  , forks_url    :: Text
  , commits_url  :: Text
  , id           :: Text
  , description  :: Text
  , public       :: Bool
  , owner        :: GistOwner
  , files        :: HashMap Text File
  , truncated    :: Bool
  , comments     :: Int
  , comments_url :: Text
  , html_url     :: Text
  , git_pull_url :: Text
  , git_push_url :: Text
  , created_at   :: UTCTime
  , updated_at   :: UTCTime
  } deriving  (Show, Eq, Generic)

instance FromJSON Gist

data GistOwner = GistOwner
  { ownerLogin :: Text
  , ownerId    :: Int
  } deriving (Show, Eq)

instance FromJSON GistOwner where
  parseJSON (Object o) = GistOwner
    <$> o .: "login"
    <*> o .: "id"
  parseJSON _ = empty

data File = File
  { fileSize      :: Int
  , fileRawUrl    :: Text
  , fileType      :: Text
  , fileTruncated :: Maybe Bool
  , fileLanguage  :: Text
  } deriving (Show, Eq)

instance FromJSON File where
  parseJSON (Object o) = File
    <$> o .:  "size"
    <*> o .:  "raw_url"
    <*> o .:  "type"
    <*> o .:? "truncated"
    <*> o .:  "language"
  parseJSON _ = empty
