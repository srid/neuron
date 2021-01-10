{-# LANGUAGE DataKinds #-}
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
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.Dependent.Map (DMap)
import Data.Dependent.Sum.Orphans ()
import Data.GADT.Compare.TH
import Data.GADT.Show.TH (DeriveGShow (deriveGShow))
import Data.Graph.Labelled (Vertex (..))
import Data.Some (Some)
import Data.TagTree (Tag, TagQuery)
import Data.Tagged (Tagged (Tagged))
import Data.Time.DateMayTime (DateMayTime)
import Neuron.Markdown (ZettelParseError)
import Neuron.Plugin.PluginData (PluginZettelData)
import Neuron.Zettelkasten.Connection (Connection)
import Neuron.Zettelkasten.ID (Slug, ZettelID)
import Neuron.Zettelkasten.Query.Theme (ZettelsView)
import Relude hiding (show)
import Text.Pandoc.Builder (Block)
import Text.Pandoc.Definition (Pandoc (..))
import Text.Show (Show (show))

-- | A zettel ID doesn't refer to an existing zettel
type MissingZettel = Tagged "MissingZettel" ZettelID

-- | ZettelQuery queries individual zettels.
--
-- It does not care about the relationship *between* those zettels; for that use `GraphQuery`.
--
-- NOTE: This type is defined in this module, rather than Zettel.Query, because
-- of the mutual dependency with the `ZettelT` type.
data ZettelQuery r where
  ZettelQuery_ZettelByID :: ZettelID -> Connection -> ZettelQuery (Either MissingZettel Zettel)
  ZettelQuery_ZettelsByTag :: TagQuery -> Connection -> ZettelsView -> ZettelQuery [Zettel]
  ZettelQuery_Tags :: TagQuery -> ZettelQuery (Map Tag Natural)
  ZettelQuery_TagZettel :: Tag -> ZettelQuery ()

-- | A zettel note
--
-- The metadata could have been inferred from the content.
data ZettelT c = Zettel
  { zettelID :: ZettelID,
    zettelSlug :: Slug,
    -- | Relative path to this zettel in the zettelkasten directory
    zettelPath :: FilePath,
    zettelTitle :: Text,
    -- | Whether the title was infered from the body
    zettelTitleInBody :: Bool,
    zettelTags :: Set Tag,
    -- | Date associated with the zettel if any
    zettelDate :: Maybe DateMayTime,
    zettelUnlisted :: Bool,
    -- | List of all queries in the zettel
    zettelQueries :: [(Some ZettelQuery, [Block])],
    zettelContent :: c,
    zettelPluginData :: DMap PluginZettelData Identity
  }
  deriving (Generic)

type MetadataOnly = Tagged "MetadataOnly" (Maybe ZettelParseError)

-- | Zettel without its content
type Zettel = ZettelT MetadataOnly

-- | Zettel that has either failed to parse, or has been parsed.
type ZettelC = Either (ZettelT (Text, ZettelParseError)) (ZettelT Pandoc)

sansContent :: ZettelC -> Zettel
sansContent = \case
  Left z ->
    z
      { zettelContent = Tagged (Just $ snd $ zettelContent z)
      }
  Right z ->
    z
      { zettelContent = Tagged Nothing
      }

-- | Strip out the link context data
--
-- Useful to to minimize the impending JSON dump.
sansLinkContext :: ZettelT c -> ZettelT c
sansLinkContext z =
  z {zettelQueries = stripContextFromZettelQuery <$> zettelQueries z}
  where
    stripContextFromZettelQuery (someQ, _ctx) = (someQ, mempty)

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
deriveGCompare ''ZettelQuery

deriveArgDict ''ZettelQuery

deriving instance Show (ZettelQuery (Maybe Zettel))

deriving instance Show (ZettelQuery [Zettel])

deriving instance Show (ZettelQuery (Map Tag Natural))

deriving instance Eq (ZettelQuery (Maybe Zettel))

deriving instance Eq (ZettelQuery [Zettel])

deriving instance Eq (ZettelQuery (Map Tag Natural))

deriving instance Eq (ZettelT Pandoc)

deriving instance Eq (ZettelT MetadataOnly)

deriving instance Eq (ZettelT (Text, ZettelParseError))

deriving instance Ord (ZettelT Pandoc)

deriving instance Ord (ZettelT MetadataOnly)

deriving instance Ord (ZettelT (Text, ZettelParseError))

deriving instance ToJSON Zettel

deriving instance FromJSON Zettel
