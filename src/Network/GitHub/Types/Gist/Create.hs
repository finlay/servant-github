{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
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
import           GHC.Generics
import qualified Network.GitHub.Types.Gist as G

data GistCreate = GistCreate
  { description :: Maybe T.Text
  , files :: HM.HashMap G.FileId FileCreate
  , public :: Maybe Bool
  } deriving (Show, Eq)

instance Monoid GistCreate where
  mempty = GistCreate
    { description = Nothing
    , files = HM.empty
    , public = Nothing
    }
  gc1 `mappend` gc2 = GistCreate
    { description = description gc1 <|> description gc2
    , files = files gc1 <> files gc2
    , public = public gc1 <|> public gc2
    }

instance ToJSON GistCreate where
  toJSON GistCreate{..} = object $
       (("description" .=) <$> maybeToList description)
    ++ (("public" .=) <$> maybeToList public)
    ++ [ "files" .= files ]

newtype FileCreate = FileCreate
  { content :: T.Text
  } deriving (Show, Eq, Monoid, Generic)

instance ToJSON FileCreate

-- | Construct a FileCreate that creates a single file
createFile :: G.FileId -> FileCreate -> GistCreate
createFile f c = mempty{ files = HM.singleton f c }
