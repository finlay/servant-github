{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import qualified Data.Text as T
import qualified Network.GitHub.Types.Gist as G

data GistEdit = GistEdit
  { description :: Maybe T.Text
  , files :: HM.HashMap G.FileId (Maybe FileEdit)
  } deriving (Show, Eq)

instance Monoid GistEdit where
  mempty = GistEdit
    { description = Nothing
    , files = HM.empty
    }
  ge1 `mappend` ge2 = GistEdit
    { description = description ge1 <|> description ge2
    -- we use the overriding behavior of map, because we need to honour the
    -- presence of Nothing in a map - these mean deletions
    , files = files ge1 <> files ge1
    }

instance ToJSON GistEdit where
  toJSON GistEdit{ description, files } = object $ fileAttrs ++ descAttrs
    where fileAttrs | HM.null files = []
                    | otherwise = [ "files" .= files ]
          descAttrs
            | Just desc <- description = ["description" .= desc]
            | otherwise = []

-- | Create a GistEdit that edits a single file
editFile :: G.FileId -> FileEdit -> GistEdit
editFile f fe = mempty{ files = HM.singleton f (Just fe) }

-- | Create a GistEdit that deletes a single file
deleteFile :: G.FileId -> GistEdit
deleteFile f = mempty{ files = HM.singleton f Nothing }

data FileEdit = FileEdit
  { filename :: Maybe T.Text
  , content  :: Maybe T.Text
  } deriving (Show, Eq)

instance Monoid FileEdit where
  mempty = FileEdit
    { filename = Nothing
    , content = Nothing
    }
  fe1 `mappend` fe2 = FileEdit
    { filename = filename fe1 <|> filename fe2
    , content = content fe1 <|> content fe2
    }

instance ToJSON FileEdit where
  toJSON FileEdit{ filename, content } = object $
       (("filename" .=) <$> maybeToList filename)
    ++ (("content"  .=) <$> maybeToList content)
