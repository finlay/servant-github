{-# LANGUAGE OverloadedStrings #-}
module Network.GitHub.Types
    ( Organisation(..)
    , OrgLogin
    , Team(..)
    , TeamId
    , Member(..)
    , MemberId
    , Repository(..)
    , RepositoryName
    )
where

import Control.Monad
import Data.Aeson
import Data.Text

-- | Organisation 
-- We only care about three of the fields: login, id, and description
type OrgLogin = Text
data Organisation = Organisation 
    { orgLogin        :: OrgLogin
    , orgId           :: Int
    , orgDescription  :: Maybe Text
    } deriving (Eq, Show)


instance FromJSON Organisation where
  parseJSON (Object o) =
   Organisation <$> o .: "login"
                <*> o .: "id"
                <*> o .: "description"
  parseJSON _ = mzero

-- | Team
type TeamId = Integer
data Team = Team
    { teamId          :: TeamId
    , teamName        :: Text
    , teamDescription :: Maybe Text
    , teamPermission  :: Maybe Text
    } deriving (Eq, Show)

instance FromJSON Team where
  parseJSON (Object o) =
   Team <$> o .: "id"
        <*> o .: "name"
        <*> o .: "description"
        <*> o .: "permission"
  parseJSON _ = mzero

-- | Member
type MemberId = Integer
data Member = Member
    { memberId        :: MemberId
    , memberLogin     :: Text
    } deriving (Eq, Show)

instance FromJSON Member where
  parseJSON (Object o) =
   Member <$> o .: "id"
          <*> o .: "login"
  parseJSON _ = mzero

-- | Repository
type RepositoryName = Text
data Repository = Repository
    { repositoryName  :: RepositoryName
    , repositoryDescription :: Maybe Text
    , repositoryPrivate :: Bool
    } deriving (Eq, Show)

instance FromJSON Repository where
  parseJSON (Object o) =
   Repository <$> o .: "name"
              <*> o .: "description"
              <*> o .: "private"
  parseJSON _ = mzero

