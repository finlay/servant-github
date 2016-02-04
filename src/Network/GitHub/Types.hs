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
    , Team(..)
    , TeamId
    , Member(..)
    , MemberId
    , Repository(..)
    , RepositoryName
    , User(..)
    )
where

import Control.Monad
import Data.Aeson
import Data.Text

-- | Organisation 
data Organisation = Organisation 
    { orgLogin        :: OrgLogin
    , orgId           :: Int
    , orgDescription  :: Maybe Text
    } deriving (Eq, Show)
-- | Primary identifier for an organisation is the login
type OrgLogin = Text


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
    , repositoryPrivate :: Bool
    } deriving (Eq, Show)
-- | repositories are identified by their name
type RepositoryName = Text

instance FromJSON Repository where
  parseJSON (Object o) =
   Repository <$> o .: "name"
              <*> o .: "description"
              <*> o .: "private"
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

