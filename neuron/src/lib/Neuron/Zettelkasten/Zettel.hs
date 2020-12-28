{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Data.Aeson.GADT.TH (deriveJSONGADT)
import Data.Dependent.Map (DMap)
import Data.Dependent.Sum.Orphans ()
import Data.GADT.Compare.TH
  ( DeriveGEQ (deriveGEq),
  )
import Data.GADT.Show.TH (DeriveGShow (deriveGShow))
import Data.Graph.Labelled (Vertex (..))
import Data.Some (Some)
import Data.TagTree (Tag, TagQuery)
import Data.Time.DateMayTime (DateMayTime)
import Neuron.Markdown (ZettelParseError)
import Neuron.Plugin.PluginData (PluginData)
import Neuron.Zettelkasten.Connection (Connection)
import Neuron.Zettelkasten.ID (Slug, ZettelID)
import Neuron.Zettelkasten.Query.Theme (ZettelsView)
import Relude hiding (show)
import Text.Pandoc.Builder (Block)
import Text.Pandoc.Definition (Pandoc (..))
import Text.Show (Show (show))

-- | ZettelQuery queries individual zettels.
--
-- It does not care about the relationship *between* those zettels; for that use `GraphQuery`.
--
-- NOTE: This type is defined in this module, rather than Zettel.Query, because
-- of the mutual dependency with the `ZettelT` type.
data ZettelQuery r where
  ZettelQuery_ZettelByID :: ZettelID -> Connection -> ZettelQuery Zettel
  ZettelQuery_ZettelsByTag :: TagQuery -> Connection -> ZettelsView -> ZettelQuery [Zettel]
  ZettelQuery_Tags :: TagQuery -> ZettelQuery (Map Tag Natural)
  ZettelQuery_TagZettel :: Tag -> ZettelQuery ()

-- | A zettel note
--
-- The metadata could have been inferred from the content.
data ZettelT content = Zettel
  { zettelID :: ZettelID,
    zettelSlug :: Slug,
    -- | Relative path to this zettel in the zettelkasten directory
    zettelPath :: FilePath,
    zettelTitle :: Text,
    -- | Whether the title was infered from the body
    zettelTitleInBody :: Bool,
    zettelTags :: [Tag],
    -- | Date associated with the zettel if any
    zettelDate :: Maybe DateMayTime,
    zettelUnlisted :: Bool,
    -- | List of all queries in the zettel
    zettelQueries :: [(Some ZettelQuery, [Block])],
    zettelParseError :: Maybe ZettelParseError,
    zettelContent :: content,
    zettelPluginData :: DMap PluginData Identity
  }
  deriving (Generic)

newtype MetadataOnly = MetadataOnly ()
  deriving (Generic, ToJSON, FromJSON)

-- | Zettel without its content
type Zettel = ZettelT MetadataOnly

-- | Zettel with its content (Pandoc or raw text)
type ZettelC = Either (ZettelT Text) (ZettelT Pandoc)

sansContent :: ZettelC -> Zettel
sansContent = \case
  Left z ->
    z
      { zettelContent = MetadataOnly ()
      }
  Right z ->
    z
      { zettelContent = MetadataOnly ()
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
  sortOn (Down . zettelDate)

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
