{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Network.GitHub.Types.Gist.Create
  ( GistCreate(..)
  , createFile
  , FileCreate(..)
  ) where

import           Control.Applicative
import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Maybe
import           Data.Monoid
import qualified Data.Text as T
import qualified Network.GitHub.Types.Gist.Core as G

data GistCreate = GistCreate
  { gistCreateDescription :: Maybe T.Text
  , gistCreateFiles       :: HM.HashMap G.FileId FileCreate
  , gistCreatePublic      :: Maybe Bool
  } deriving (Show, Eq)

instance Monoid GistCreate where
  mempty = GistCreate
    { gistCreateDescription = Nothing
    , gistCreateFiles       = HM.empty
    , gistCreatePublic      = Nothing
    }
  gc1 `mappend` gc2 = GistCreate
    { gistCreateDescription = gistCreateDescription gc1 <|> gistCreateDescription gc2
    , gistCreateFiles       = gistCreateFiles gc1 <> gistCreateFiles gc2
    , gistCreatePublic      = gistCreatePublic gc1 <|> gistCreatePublic gc2
    }

instance ToJSON GistCreate where
  toJSON GistCreate{..} = object $
       (("description" .=) <$> maybeToList gistCreateDescription)
    ++ (("public" .=) <$> maybeToList gistCreatePublic)
    ++ [ "files" .= gistCreateFiles ]

newtype FileCreate = FileCreate
  { fileCreateContent :: T.Text
  } deriving (Show, Eq, Monoid)

instance ToJSON FileCreate where
  toJSON FileCreate{..} = object [ "content" .= fileCreateContent ]

-- | Construct a FileCreate that creates a single file
createFile :: G.FileId -> FileCreate -> GistCreate
createFile f c = mempty{ gistCreateFiles = HM.singleton f c }
