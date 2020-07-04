{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Zettel where

import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Dependent.Sum.Orphans ()
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Data.Graph.Labelled (Vertex (..))
import Data.Some
import Data.TagTree (Tag)
import Data.TagTree (TagPattern (..))
import Data.Time.Calendar
import Neuron.Reader.Type
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Query.Error
import Neuron.Zettelkasten.Query.Theme
import Relude hiding (show)
import Text.Pandoc.Definition (Pandoc (..))
import Text.Show (Show (show))

-- | ZettelQuery queries individual zettels.
--
-- It does not care about the relationship *between* those zettels; for that use `GraphQuery`.
data ZettelQuery r where
  ZettelQuery_ZettelByID :: ZettelID -> Maybe Connection -> ZettelQuery Zettel
  ZettelQuery_ZettelsByTag :: [TagPattern] -> Maybe Connection -> ZettelsView -> ZettelQuery [Zettel]
  ZettelQuery_Tags :: [TagPattern] -> ZettelQuery (Map Tag Natural)

-- | A zettel note
--
-- The metadata could have been inferred from the content.
data ZettelT content = Zettel
  { zettelID :: ZettelID,
    zettelFormat :: ZettelFormat,
    -- Relative path to this zettel in the zettelkasten directory
    zettelPath :: FilePath,
    zettelTitle :: Text,
    zettelTitleInBody :: Bool,
    zettelTags :: [Tag],
    zettelDay :: Maybe Day,
    zettelQueries :: [Some ZettelQuery],
    zettelError :: ContentError content,
    zettelContent :: content
  }
  deriving (Generic)

newtype MetadataOnly = MetadataOnly ()
  deriving (Generic, ToJSON, FromJSON)

type family ContentError c where
-- The list of queries that failed to parse.
  ContentError Pandoc = [QueryParseError]
-- When a zettel fails to parse, we use its raw text along with its parse error.
  ContentError Text = ZettelParseError
-- When working with zettel sans content, we gather both kinds of errors (above)
  ContentError MetadataOnly = Either (ContentError Text) (ContentError Pandoc)

-- | All possible errors in a zettel
--
-- NOTE: Unlike `ContentError MetadataOnly` this also includes QueryResultError
-- (which can be determined only after *evaluating* the queries).
data ZettelError
  = ZettelError_ParseError ZettelParseError
  | ZettelError_QueryErrors (NonEmpty QueryError)
  | ZettelError_AmbiguousFiles (NonEmpty FilePath)
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Zettel without its content
type Zettel = ZettelT MetadataOnly

-- | Zettel with its content (Pandoc or raw text)
type ZettelC = Either (ZettelT Text) (ZettelT Pandoc)

sansContent :: ZettelC -> Zettel
sansContent = \case
  Left z ->
    z
      { zettelError = Left $ zettelError z,
        zettelContent = MetadataOnly ()
      }
  Right z ->
    z
      { zettelError = Right $ zettelError z,
        zettelContent = MetadataOnly ()
      }

instance Eq (ZettelT c) where
  (==) = (==) `on` zettelID

instance Ord (ZettelT c) where
  compare = compare `on` zettelID

instance Show (ZettelT c) where
  show Zettel {..} = "Zettel:" <> show zettelID

instance Vertex (ZettelT c) where
  type VertexID (ZettelT c) = ZettelID
  vertexID = zettelID

sortZettelsReverseChronological :: [Zettel] -> [Zettel]
sortZettelsReverseChronological =
  sortOn (Down . zettelDay)

deriveJSONGADT ''ZettelQuery

deriveGEq ''ZettelQuery

deriveGShow ''ZettelQuery

deriving instance Show (ZettelQuery (Maybe Zettel))

deriving instance Show (ZettelQuery [Zettel])

deriving instance Show (ZettelQuery (Map Tag Natural))

deriving instance Eq (ZettelQuery (Maybe Zettel))

deriving instance Eq (ZettelQuery [Zettel])

deriving instance Eq (ZettelQuery (Map Tag Natural))

deriving instance ToJSON Zettel

deriving instance FromJSON Zettel
