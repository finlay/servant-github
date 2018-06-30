{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Types for editing a Gist
module Network.GitHub.Types.Gist.Edit
  ( GistEdit(..)
  , editFile
  , deleteFile
  , FileEdit(..)
  ) where

import           Control.Applicative
import           Data.Aeson
import qualified Data.HashMap.Strict as HM
import           Data.Maybe
import           Data.Monoid
import           Data.Semigroup as Sem
import qualified Data.Text as T
import qualified Network.GitHub.Types.Gist.Core as G

data GistEdit = GistEdit
  { gistEditDescription :: Maybe T.Text
  , gistEditFiles :: HM.HashMap G.FileId (Maybe FileEdit)
  } deriving (Show, Eq)


instance Sem.Semigroup GistEdit where
  ge1 <> ge2 = GistEdit
    { gistEditDescription = gistEditDescription ge1 <|> gistEditDescription ge2
    -- we use the overriding behavior of map, because we need to honour the
    -- presence of Nothing in a map - these mean deletions
    , gistEditFiles = gistEditFiles ge1 <> gistEditFiles ge1
    }

instance Monoid GistEdit where
  mempty = GistEdit
    { gistEditDescription = Nothing
    , gistEditFiles = HM.empty
    }
#if !(MIN_VERSION_base(4,11,0))
  mappend = (<>)
#endif

instance ToJSON GistEdit where
  toJSON GistEdit{ gistEditDescription=description, gistEditFiles=files }
    = object $ fileAttrs ++ descAttrs
    where fileAttrs | HM.null files = []
                    | otherwise = [ "files" .= files ]
          descAttrs
            | Just desc <- description = ["description" .= desc]
            | otherwise = []

data FileEdit = FileEdit
  { fileEditFilename :: Maybe T.Text
  , fileEditContent  :: Maybe T.Text
  } deriving (Show, Eq)

instance Sem.Semigroup FileEdit where
  fe1 <> fe2 = FileEdit
    { fileEditFilename = fileEditFilename fe1 <|> fileEditFilename fe2
    , fileEditContent = fileEditContent fe1 <|> fileEditContent fe2
    }

instance Monoid FileEdit where
  mempty = FileEdit
    { fileEditFilename = Nothing
    , fileEditContent = Nothing
    }
#if !(MIN_VERSION_base(4,11,0))
  mappend = (<>)
#endif

instance ToJSON FileEdit where
  toJSON FileEdit{..} = object $
       (("filename" .=) <$> maybeToList fileEditFilename)
    ++ (("content"  .=) <$> maybeToList fileEditContent)

-- | Create a GistEdit that edits a single file
editFile :: G.FileId -> FileEdit -> GistEdit
editFile f fe = mempty{ gistEditFiles = HM.singleton f (Just fe) }

-- | Create a GistEdit that deletes a single file
deleteFile :: G.FileId -> GistEdit
deleteFile f = mempty{ gistEditFiles = HM.singleton f Nothing }
