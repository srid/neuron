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
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Zettel where

import Data.Aeson hiding ((.:))
import Data.Aeson.GADT.TH (deriveJSONGADT)
import Data.Char (isLower, toLower)
import Data.Constraint.Extras.TH (deriveArgDict)
import Data.Default
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum.Orphans ()
import Data.GADT.Compare.TH
import Data.GADT.Show.TH (DeriveGShow (deriveGShow))
import Data.Graph.Labelled (Vertex (..))
import Data.Some (Some)
import Data.TagTree (Tag)
import qualified Data.TagTree as TagTree
import Data.Tagged (Tagged)
import Data.Time.DateMayTime (DateMayTime)
import Neuron.Markdown (ZettelMeta, ZettelParseError)
import Neuron.Zettelkasten.Connection (Connection)
import Neuron.Zettelkasten.ID (Slug, ZettelID, unZettelID)
import Relude hiding (show)
import Text.Pandoc.Builder (Block)
import Text.Pandoc.Definition (Pandoc (..))
import Text.Show (Show (show))

-- ------------
-- Plugin types
-- ------------

-- NOTE: Ideally we want to put this in Plugin modules, but there is a mutual
-- dependency with the Zettel type. :/

-- | Metadata part for dirtree plugin
data DirTreeMeta = DirTreeMeta
  { dirtreemetaDisplay :: Bool
  }
  deriving (Eq, Ord, Show, Generic)

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

data FeedMeta = FeedMeta
  {feedmetaCount :: Natural}
  deriving (Eq, Ord, Show, Generic)

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
  Tags :: PluginZettelData [Some TagQuery]
  NeuronIgnore :: PluginZettelData ()
  UpTree :: PluginZettelData ()
  Feed :: PluginZettelData FeedMeta

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
    zettelMeta :: ZettelMeta,
    -- Slug is non-changing - so, although inferred from zettelMeta, we must
    -- put it here as a data type field.
    zettelSlug :: Slug, -- inferred from zettelMeta
    -- Since date is used as a sort key, we parse it once from zettelMeta for
    -- performance reasons.
    zettelDate :: Maybe DateMayTime, -- inferred from zettelMeta

    -- | Relative path to this zettel in the zettelkasten directory
    zettelPath :: FilePath,
    zettelTitle :: Text,
    zettelContent :: c,
    -- This type is a Maybe only so that we can use omitNothingFields to strip
    -- it off the output JSON.
    zettelPluginData :: Maybe (DMap PluginZettelData Identity)
  }
  deriving (Generic)

type MetadataOnly = (Maybe ZettelParseError)

-- | Zettel without its content
type Zettel = ZettelT MetadataOnly

-- | Zettel that has either failed to parse, or has been parsed.
type ZettelC = Either (ZettelT (Text, ZettelParseError)) (ZettelT Pandoc)

setPluginData :: PluginZettelData v -> v -> ZettelT c -> ZettelT c
setPluginData k v z =
  z
    { zettelPluginData =
        maybe
          (Just $ DMap.singleton k (Identity v))
          (Just . DMap.insert k (Identity v))
          (zettelPluginData z)
    }

lookupPluginData :: PluginZettelData a -> ZettelT c -> Maybe a
lookupPluginData k z = do
  pd <- zettelPluginData z
  Identity v <- DMap.lookup k pd
  pure v

sansContent :: ZettelC -> Zettel
sansContent = \case
  Left z ->
    z
      { zettelContent = Just $ snd $ zettelContent z
      }
  Right z ->
    z
      { zettelContent = Nothing
      }

instance Show (ZettelT c) where
  show Zettel {..} = toString $ "Zettel:" <> unZettelID zettelID

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

deriving instance Eq (ZettelT Pandoc)

deriving instance Eq (ZettelT MetadataOnly)

deriving instance Eq (ZettelT (Text, ZettelParseError))

instance ToJSON DirTreeMeta where
  toJSON = genericToJSON shortRecordFieldsLowerCase

instance FromJSON DirTreeMeta where
  parseJSON = genericParseJSON shortRecordFieldsLowerCase

instance ToJSON FeedMeta where
  toJSON = genericToJSON shortRecordFieldsLowerCase

instance FromJSON FeedMeta where
  parseJSON = genericParseJSON shortRecordFieldsLowerCase

instance ToJSON DirZettel where
  toJSON = genericToJSON shortRecordFields

instance FromJSON DirZettel where
  parseJSON = genericParseJSON shortRecordFields

instance ToJSON Zettel where
  toJSON =
    genericToJSON
      shortRecordFields
        { omitNothingFields = True
        }

instance FromJSON Zettel where
  parseJSON = genericParseJSON shortRecordFields

shortRecordFields :: Options
shortRecordFields = shortRecordFields' False

shortRecordFieldsLowerCase :: Options
shortRecordFieldsLowerCase = shortRecordFields' True

shortRecordFields' :: Bool -> Options
shortRecordFields' lowerCase =
  defaultOptions
    { fieldLabelModifier =
        \case
          -- Drop the "_foo_" prefix
          '_' : rest -> drop 1 $ dropWhile (/= '_') rest
          -- Drop "zettel" prefix
          s -> bool id (fmap toLower) lowerCase $ dropWhile isLower s
    }
