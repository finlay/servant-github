{-# LANGUAGE OverloadedStrings #-}
module Network.GitHub.Organisation
    ( Organisation(..)
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
data Organisation = Organisation 
    { orgLogin        :: Text
    , orgId           :: Int
    , orgDescription  :: Maybe Text
    } deriving (Eq, Show)


instance FromJSON Organisation where
  parseJSON (Object o) =
   Organisation <$> o .: "login"
                <*> o .: "id"
                <*> o .: "description"
  parseJSON _ = mzero

