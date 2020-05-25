{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
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
  ZettelQuery_ZettelByID :: ZettelID -> Maybe Connection -> ZettelQuery (Maybe Zettel)
  ZettelQuery_ZettelsByTag :: [TagPattern] -> Maybe Connection -> ZettelsView -> ZettelQuery [Zettel]
  ZettelQuery_Tags :: [TagPattern] -> ZettelQuery (Map Tag Natural)

-- | Zettel with no associated content
--
-- The metadata could have been inferred from the content.
data Zettel = Zettel
  { zettelID :: ZettelID,
    zettelTitle :: Text,
    zettelTags :: [Tag],
    zettelDay :: Maybe Day,
    zettelQueries :: ([Some ZettelQuery], [QueryParseError])
  }
  deriving (Generic)

-- | A zettel with the associated pandoc AST
newtype PandocZettel = PandocZettel {unPandocZettel :: (Zettel, Pandoc)}
  deriving (Eq)

instance Eq Zettel where
  (==) = (==) `on` zettelID

instance Ord Zettel where
  compare = compare `on` zettelID

instance Show Zettel where
  show Zettel {..} = "Zettel:" <> show zettelID

instance Vertex Zettel where
  type VertexID Zettel = ZettelID
  vertexID = zettelID

sortZettelsReverseChronological :: [Zettel] -> [Zettel]
sortZettelsReverseChronological =
  sortOn (Down . zettelDay)

-- TODO: Remove this in favour of the ToJSON instance (which now includes queries).
-- That will affect the `neuron query` output, as well neuron-mode.
zettelJson :: forall a. KeyValue a => Zettel -> [a]
zettelJson Zettel {..} =
  [ "id" .= toJSON zettelID,
    "title" .= zettelTitle,
    "tags" .= zettelTags,
    "day" .= zettelDay
  ]

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
