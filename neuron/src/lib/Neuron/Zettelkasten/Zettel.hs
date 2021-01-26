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

import Data.Aeson hiding ((.:))
import Data.Aeson.GADT.TH (deriveJSONGADT)
import Data.Char (isLower)
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.Default
import Data.Dependent.Map (DMap)
import Data.Dependent.Sum.Orphans ()
import Data.GADT.Compare.TH
import Data.GADT.Show.TH (DeriveGShow (deriveGShow))
import Data.Graph.Labelled (Vertex (..))
import Data.Some (Some)
import Data.TagTree (Tag)
import qualified Data.TagTree as TagTree
import Data.Tagged (Tagged (Tagged))
import Data.Time.DateMayTime (DateMayTime)
import Data.YAML (FromYAML (parseYAML), (.:))
import qualified Data.YAML as Y
import Neuron.Markdown (ZettelParseError)
import Neuron.Zettelkasten.Connection (Connection)
import Neuron.Zettelkasten.ID (Slug, ZettelID)
import Relude hiding (show)
import Text.Pandoc.Builder (Block)
import Text.Pandoc.Definition (Pandoc (..))
import Text.Show (Show (show))

-- ------------
-- Plugin types
-- ------------

-- NOTE: Ideally we want to put this in Plugin modules, but there is a mutual
-- dependency with the Zettel type. :/

data DirTreeMeta = DirTreeMeta
  { dirtreemetaDisplay :: Bool
  }
  deriving (Eq, Ord, Show, Generic)

instance Y.FromYAML DirTreeMeta where
  parseYAML =
    Y.withMap "Meta" $ \m ->
      DirTreeMeta
        <$> m .: "display"

instance Default DirTreeMeta where
  def = DirTreeMeta True

-- | Extended metadata on Zettel managed by DirTree plugin
data DirZettel = DirZettel
  { -- | What to tag this zettel.
    -- We expect the arity here to be 1-2. 1 for the simplest case; and 2, if
    -- both Foo/ and Foo.md exists, with the later being positioned *elsewhere*
    -- in the tree, with its own parent directory.
    _dirZettel_tags :: Set Tag,
    -- | The directory zettel associated with the parent directory if any.
    _dirZettel_dirParent :: Maybe ZettelID,
    -- | The tag used by its child zettels (directories and files) if any.
    -- This is Nothing for "terminal" zettels
    _dirZettel_childrenTag :: Maybe Tag,
    _dirZettel_meta :: Maybe DirTreeMeta
  }
  deriving (Eq, Ord, Show, Generic)

data ZettelsView = ZettelsView
  { zettelsviewLinkView :: LinkView,
    zettelsviewGroupByTag :: Bool,
    zettelsviewLimit :: Maybe Natural
  }
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

data LinkView
  = LinkView_Default
  | LinkView_ShowDate
  | LinkView_ShowID
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

instance Default LinkView where
  def = LinkView_Default

instance Default ZettelsView where
  def = ZettelsView def False def

data TagQuery r where
  TagQuery_ZettelsByTag :: TagTree.Query -> Connection -> ZettelsView -> TagQuery [Zettel]
  TagQuery_Tags :: TagTree.Query -> TagQuery (Map Tag Natural)
  TagQuery_TagZettel :: Tag -> TagQuery ()

data ZettelTags = ZettelTags
  { zetteltagsTagged :: Set Tag,
    zetteltagsQueries :: [Some TagQuery]
  }
  deriving (Generic)

-- | Plugin-specific data stored in `ZettelT`
--
-- See also `PluginZettelRouteData` which corresponds to post-graph data (used
-- in rendering).
--
-- NOTE: The constructors deliberately are kept short, so as to have shorter
-- JSON
data PluginZettelData a where
  DirTree :: PluginZettelData DirZettel
  Links :: PluginZettelData [((ZettelID, Connection), [Block])]
  Tags :: PluginZettelData ZettelTags
  NeuronIgnore :: PluginZettelData ()
  UpTree :: PluginZettelData ()

-- ------------
-- Zettel types
-- ------------

-- | A zettel ID doesn't refer to an existing zettel
type MissingZettel = Tagged "MissingZettel" ZettelID

-- | A zettel note
--
-- The metadata could have been inferred from the content.
data ZettelT c = Zettel
  { zettelID :: ZettelID,
    zettelSlug :: Slug,
    -- | Relative path to this zettel in the zettelkasten directory
    zettelPath :: FilePath,
    zettelTitle :: Text,
    -- | Whether the title was infered from the body. Used when conditionally
    -- rendering the title in HTML.
    zettelTitleInBody :: Bool,
    -- | Date associated with the zettel if any
    zettelDate :: Maybe DateMayTime,
    zettelUnlisted :: Bool,
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

instance Show (ZettelT c) where
  show Zettel {..} = "Zettel:" <> show zettelID

instance Eq (ZettelT c) => Ord (ZettelT c) where
  z1 <= z2 =
    (zettelDate z1, zettelID z1) <= (zettelDate z2, zettelID z2)

instance Vertex (ZettelT c) where
  type VertexID (ZettelT c) = ZettelID
  vertexID = zettelID

sortZettelsReverseChronological :: [Zettel] -> [Zettel]
sortZettelsReverseChronological =
  sortOn (Down . zettelDate)

deriveJSONGADT ''TagQuery
deriveGEq ''TagQuery
deriveGShow ''TagQuery
deriveGCompare ''TagQuery
deriveArgDict ''TagQuery

deriveArgDict ''PluginZettelData
deriveJSONGADT ''PluginZettelData
deriveGEq ''PluginZettelData
deriveGShow ''PluginZettelData
deriveGCompare ''PluginZettelData

deriving instance Eq ZettelTags

deriving instance Ord ZettelTags

instance ToJSON ZettelTags where
  toJSON = genericToJSON shortRecordFields

instance FromJSON ZettelTags where
  parseJSON = genericParseJSON shortRecordFields

deriving instance Show ZettelTags

deriving instance Eq (ZettelT Pandoc)

deriving instance Eq (ZettelT MetadataOnly)

deriving instance Eq (ZettelT (Text, ZettelParseError))

instance ToJSON DirTreeMeta where
  toJSON = genericToJSON shortRecordFields

instance FromJSON DirTreeMeta where
  parseJSON = genericParseJSON shortRecordFields

instance ToJSON DirZettel where
  toJSON = genericToJSON shortRecordFields

instance FromJSON DirZettel where
  parseJSON = genericParseJSON shortRecordFields

instance ToJSON Zettel where
  toJSON = genericToJSON shortRecordFields

instance FromJSON Zettel where
  parseJSON = genericParseJSON shortRecordFields

shortRecordFields :: Options
shortRecordFields =
  defaultOptions
    { fieldLabelModifier =
        \case
          -- Drop the "_foo_" prefix
          '_' : rest -> drop 1 $ dropWhile (/= '_') rest
          -- Drop "zettel" prefix
          s -> dropWhile isLower s
    }
