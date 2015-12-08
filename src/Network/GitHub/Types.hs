{-# LANGUAGE OverloadedStrings #-}
module Network.GitHub.Types
    ( Organisation(..)
    , Team(..)
    , OrgLogin
    )
where

import Control.Monad
import Data.Aeson
import Data.Text

{--
  GET /user/orgs
  [ 
   {
    "login": "github",
    "id": 1,
    "url": "https://api.github.com/orgs/github",
    "avatar_url": "https://github.com/images/error/octocat_happy.gif",
    "description": "A great organization"
   } 
  ]
--}

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
data Team = Team
    { teamId          :: Integer
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

