{-# LANGUAGE OverloadedStrings #-}
module Network.GitHub.Types.Gist.Core
  ( GistId
  , FileId
  , Gist(..)
  , GistOwner(..)
  , File(..)
  ) where

import Control.Applicative
import Data.Aeson
import Data.HashMap.Strict (HashMap)
import Data.Text (Text)
import Data.Time.Clock

--- Gist Types
type GistId = Text
type FileId = Text
data Gist = Gist
  { gistUrl         :: Text
  , gistForksUrl    :: Text
  , gistCommitsUrl  :: Text
  , gistId          :: GistId
  , gistDescription :: Maybe Text
  , gistPublic      :: Bool
  , gistOwner       :: GistOwner
  , gistFiles       :: HashMap FileId File
  , gistTruncated   :: Bool
  , gistComments    :: Int
  , gistCommentsUrl :: Text
  , gistHtmlUrl     :: Text
  , gistGitPullUrl  :: Text
  , gistGitPushUrl  :: Text
  , gistCreatedAt   :: UTCTime
  , gistUpdatedAt   :: UTCTime
  } deriving  (Show, Eq)

instance FromJSON Gist where
  parseJSON (Object o) = Gist
    <$> o .: "url"
    <*> o .: "forks_url"
    <*> o .: "commits_url"
    <*> o .: "id"
    <*> o .: "description"
    <*> o .: "public"
    <*> o .: "owner"
    <*> o .: "files"
    <*> o .: "truncated"
    <*> o .: "comments"
    <*> o .: "comments_url"
    <*> o .: "html_url"
    <*> o .: "git_pull_url"
    <*> o .: "git_push_url"
    <*> o .: "created_at"
    <*> o .: "updated_at"
  parseJSON _ = empty

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
