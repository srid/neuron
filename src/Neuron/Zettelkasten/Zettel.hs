{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Zettel where

import Data.Aeson
import Data.Graph.Labelled (Vertex (..))
import Data.TagTree (Tag)
import Data.Time.Calendar
import Development.Shake
import Neuron.Zettelkasten.ID
import qualified Neuron.Zettelkasten.Zettel.Meta as Meta
import Relude hiding (show)
import qualified Rib.Parser.MMark as MMark
import Text.MMark (MMark)
import Text.Show (Show (show))

data ZettelT content = Zettel
  { zettelID :: ZettelID,
    zettelTitle :: Text,
    zettelTags :: [Tag],
    zettelDay :: Maybe Day,
    zettelContent :: content
  }

type Zettel = ZettelT MMark

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

zettelJson :: forall a c. KeyValue a => ZettelT c -> [a]
zettelJson Zettel {..} =
  [ "id" .= toJSON zettelID,
    "title" .= zettelTitle,
    "tags" .= zettelTags,
    "day" .= zettelDay
  ]

-- | Load a zettel from a file.
mkZettelFromPath :: FilePath -> Action Zettel
mkZettelFromPath path = do
  -- Extensions are computed and applied during rendering, not here.
  let noExts = []
  doc <- MMark.parseWith noExts path
  let zid = mkZettelID path
      meta = Meta.getMeta doc
      title = maybe (toText $ "No title for " <> path) Meta.title meta
      tags = fromMaybe [] $ Meta.tags =<< meta
      day = case zid of
        -- We ignore the "data" meta field on legacy Date IDs, which encode the
        -- creation date in the ID.
        ZettelDateID v _ -> Just v
        ZettelCustomID _ -> Meta.date =<< meta
  pure $ Zettel zid title tags day doc

-- TODO: This is used in another project; and it needs to be refactored with the
-- other function once stable.
mkZettelFromMarkdown :: ZettelID -> Text -> Either Text (ZettelT ())
mkZettelFromMarkdown zid s = do
  doc <- MMark.parsePureWith [] "<mkZettelFromMarkdown>" s
  let meta = Meta.getMeta doc
      title = maybe "Missing title" Meta.title meta
      tags = fromMaybe [] $ Meta.tags =<< meta
      day = case zid of
        -- We ignore the "data" meta field on legacy Date IDs, which encode the
        -- creation date in the ID.
        ZettelDateID v _ -> Just v
        ZettelCustomID _ -> Meta.date =<< meta
  pure $
    Zettel zid title tags day ()
