{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}
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
import           Data.Semigroup as Sem
import qualified Data.Text as T
import qualified Network.GitHub.Types.Gist.Core as G

data GistCreate = GistCreate
  { gistCreateDescription :: Maybe T.Text
  , gistCreateFiles       :: HM.HashMap G.FileId FileCreate
  , gistCreatePublic      :: Maybe Bool
  } deriving (Show, Eq)

instance Sem.Semigroup GistCreate where
  gc1 <> gc2 = GistCreate
    { gistCreateDescription = gistCreateDescription gc1 <|> gistCreateDescription gc2
    , gistCreateFiles       = gistCreateFiles gc1 <> gistCreateFiles gc2
    , gistCreatePublic      = gistCreatePublic gc1 <|> gistCreatePublic gc2
    }

instance Monoid GistCreate where
  mempty = GistCreate
    { gistCreateDescription = Nothing
    , gistCreateFiles       = HM.empty
    , gistCreatePublic      = Nothing
    }
#if !(MIN_VERSION_base(4,11,0))
  mappend = (<>)
#endif

instance ToJSON GistCreate where
  toJSON GistCreate{..} = object $
       (("description" .=) <$> maybeToList gistCreateDescription)
    ++ (("public" .=) <$> maybeToList gistCreatePublic)
    ++ [ "files" .= gistCreateFiles ]

newtype FileCreate = FileCreate
  { fileCreateContent :: T.Text
  } deriving (Show, Eq, Sem.Semigroup, Monoid)

instance ToJSON FileCreate where
  toJSON FileCreate{..} = object [ "content" .= fileCreateContent ]

-- | Construct a FileCreate that creates a single file
createFile :: G.FileId -> FileCreate -> GistCreate
createFile f c = mempty{ gistCreateFiles = HM.singleton f c }
