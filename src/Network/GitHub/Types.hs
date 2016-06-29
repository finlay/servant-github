{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Network.GitHub.Types
-- Copyright   : (c) Finlay Thompson, 2015
-- License     : BSD3
-- Maintainer  : finlay.thompson@gmail.com
-- Stability   : experimental
--
-- Most of the types only parse part of the data availble in the return
-- values from the GitHub API. These will be added to as required.
 
module Network.GitHub.Types
    ( Organisation(..)
    , OrgLogin
    , Owner
    , UserLogin(..)
    , Team(..)
    , TeamId
    , Member(..)
    , MemberId
    , Repository(..)
    , RepositoryName
    , User(..)
    , RepoName -- short version
    , Sha
    , Commit(..)
    , Content(..)
    , Issue(..)
    , Label(..)
    , Milestone(..)
    )
where

import Control.Monad
import GHC.Generics

import Data.Aeson
import Data.Text
import Data.Time

-- | Organisation 
data Organisation = Organisation 
    { orgLogin        :: OrgLogin
    , orgId           :: Int
    , orgDescription  :: Maybe Text
    } deriving (Eq, Show)
-- | Primary identifier for an organisation is the login
type OrgLogin = Text

type Owner = Text


instance FromJSON Organisation where
  parseJSON (Object o) =
   Organisation <$> o .: "login"
                <*> o .: "id"
                <*> o .: "description"
  parseJSON _ = mzero

-- | Team
data Team = Team
    { teamId          :: TeamId
    , teamName        :: Text
    , teamDescription :: Maybe Text
    , teamPermission  :: Maybe Text
    } deriving (Eq, Show)
-- | Identifier for a team id
type TeamId = Integer

instance FromJSON Team where
  parseJSON (Object o) =
   Team <$> o .: "id"
        <*> o .: "name"
        <*> o .: "description"
        <*> o .: "permission"
  parseJSON _ = mzero

-- | Member
data Member = Member
    { memberId        :: MemberId
    , memberLogin     :: Text
    } deriving (Eq, Show)
-- | members are identified by ids
type MemberId = Integer

instance FromJSON Member where
  parseJSON (Object o) =
   Member <$> o .: "id"
          <*> o .: "login"
  parseJSON _ = mzero

-- | Repository
data Repository = Repository
    { repositoryName  :: RepositoryName
    , repositoryDescription :: Maybe Text
    , repositoryDefaultBranch :: Maybe Text
    , repositoryPrivate :: Bool
    , repositoryPermissions :: Maybe Permission
    } deriving (Eq, Show)
-- | repositories are identified by their name
type RepositoryName = Text

data Permission = Push | Pull | Admin deriving (Eq)
instance Show Permission where
    show Push  = "push"
    show Pull  = "pull"
    show Admin = "admin"
instance FromJSON Permission where
  parseJSON (Object o) = do 
    admin <- o .: "admin"
    push  <- o .: "push"
    return $ if admin then Admin
              else if push then Push
              else Pull
  parseJSON _ = mzero

instance FromJSON Repository where
  parseJSON (Object o) =
   Repository <$> o .: "full_name"
              <*> o .:? "description"
              <*> o .:? "default_branch"
              <*> o .: "private"
              <*> o .:? "permissions"
  parseJSON _ = mzero

-- | Organisation 
data User = User
    { userLogin       :: Text
    , userId          :: Int
    , userName        :: Maybe Text
    , userCompany     :: Maybe Text
    , userEmail       :: Maybe Text
    } deriving (Eq, Show)

instance FromJSON User where
  parseJSON (Object o) =
   User <$> o .: "login"
        <*> o .: "id"
        <*> o .: "name"
        <*> o .: "company"
        <*> o .: "email"
  parseJSON _ = mzero

instance ToJSON User where
  toJSON u = 
    object [ "login"   .= userLogin u
           , "id"      .= userId u
           , "name"    .= userName u
           , "company" .= userCompany u
           , "email"   .= userEmail u
           ]

-- | Commit
type RepoName = Text
type Sha = Text
data Commit = Commit
    { commitMessage     :: Text
    , commitUrl         :: Text
    } deriving (Eq, Show)

-- TODO: fix this instance
instance FromJSON Commit where
  parseJSON (Object o) = do
        html_url <- o .: "html_url"
        commit   <- o .: "commit"
        message <- commit .: "message"
        return $ Commit message html_url
  parseJSON _ = mzero

instance ToJSON Commit where
  toJSON c = 
    let head_commit = object [ "message" .= commitMessage c
                             , "url"     .= commitUrl c ]
    in object [ "commits"     .= toJSON [ head_commit ]
              , "head_commit" .= head_commit ]

-- | Content
data Content = Content
    { contentType     :: Text
    , contentEncoding :: Text
    , contentSize     :: Int
    , contentName     :: Text
    , contentPath     :: Text
    , contentContent  :: Text
    } deriving (Eq, Show)

instance FromJSON Content where
    parseJSON (Object o) =
        Content <$> o .: "type"
                <*> o .: "encoding"
                <*> o .: "size"
                <*> o .: "name"
                <*> o .: "path"
                <*> o .: "content"
    parseJSON _ = mzero

newtype UserLogin = UserLogin Text deriving (Show, Eq)
instance FromJSON UserLogin where
    parseJSON (Object o) =
        UserLogin <$> o .: "login"
    parseJSON _ = mzero

data Milestone = Milestone
    { milestoneNumber        :: Int
    , milestoneState         :: Text
    , milestoneTitle         :: Text
    , milestoneDescripiton   :: Text
    , milestoneCreator       :: UserLogin
    , milestoneOpenIssues    :: Int
    , milestoneClosedIssues  :: Int
    , milestoneCreated       :: UTCTime
    , milestoneUpdated       :: Maybe UTCTime
    , milestoneClosed        :: Maybe UTCTime
    , milestoneDueOn         :: Maybe UTCTime
    } deriving (Show, Eq)
instance FromJSON Milestone where
    parseJSON (Object o) =
        Milestone <$> o .: "number"
                  <*> o .: "state" 
                  <*> o .: "title" 
                  <*> o .: "description" 
                  <*> o .: "creator" 
                  <*> o .: "open_issues" 
                  <*> o .: "closed_issues" 
                  <*> o .: "created_at" 
                  <*> o .: "updated_at" 
                  <*> o .: "closed_at" 
                  <*> o .: "due_on" 
    parseJSON _ = mzero

newtype Label = Label Text deriving (Eq, Show)
instance FromJSON Label where
    parseJSON (Object o) =
        Label <$> o .: "name"
    parseJSON _ = mzero

-- | Issue
data Issue = Issue
    { issueNumber     :: Int
    , issueUrl        :: Text
    , issueState      :: Text
    , issueTitle      :: Text
    , issueBody       :: Text
    , issueUser       :: UserLogin
    , issueAssignee   :: Maybe UserLogin
    , issueMilestone  :: Maybe Milestone
    , issueLabels     :: [Label]
    , issueLocked     :: Bool
    , issueComments   :: Int
    , issueCreated    :: UTCTime
    , issueUpdated    :: UTCTime
    , issueClosed     :: Maybe UTCTime
    } deriving (Generic, Eq, Show)

instance FromJSON Issue where
    parseJSON (Object o) =
        Issue   <$> o .: "number"
                <*> o .: "html_url"
                <*> o .: "state"
                <*> o .: "title"
                <*> o .: "body"
                <*> o .: "user"
                <*> o .:? "assignee"
                <*> o .:? "milestone"
                <*> o .: "labels"
                <*> o .: "locked"
                <*> o .: "comments"
                <*> o .: "created_at"
                <*> o .: "updated_at"
                <*> o .: "closed_at"
    parseJSON _ = mzero

