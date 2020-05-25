{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Zettel where

import Data.Aeson
import Data.Graph.Labelled (Vertex (..))
import qualified Data.Map.Strict as Map
import Data.TagTree (Tag)
import Data.Time.Calendar
import Neuron.Markdown
import Neuron.Zettelkasten.ID
import qualified Neuron.Zettelkasten.Zettel.Meta as Meta
import Reflex.Class (filterLeft, filterRight)
import Relude hiding (show)
import Text.Pandoc.Definition (Pandoc (..))
import Text.Show (Show (show))

-- | Zettel with no associated content
--
-- The metadata could have been inferred from the content.
data Zettel = Zettel
  { zettelID :: ZettelID,
    zettelTitle :: Text,
    zettelTags :: [Tag],
    zettelDay :: Maybe Day
  }
  deriving (Generic, ToJSON)

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

zettelJson :: forall a. KeyValue a => Zettel -> [a]
zettelJson Zettel {..} =
  [ "id" .= toJSON zettelID,
    "title" .= zettelTitle,
    "tags" .= zettelTags,
    "day" .= zettelDay
  ]

-- | Parse a markdown-formatted zettel
--
-- In future this will support other formats supported by Pandoc.
parseZettel ::
  ZettelID ->
  Text ->
  Either Text PandocZettel
parseZettel zid s = do
  (meta, doc) <- parseMarkdown (zettelIDSourceFileName zid) s
  let title = maybe "Missing title" Meta.title meta
      tags = fromMaybe [] $ Meta.tags =<< meta
      day = case zid of
        -- We ignore the "data" meta field on legacy Date IDs, which encode the
        -- creation date in the ID.
        ZettelDateID v _ -> Just v
        ZettelCustomID _ -> Meta.date =<< meta
  pure $ PandocZettel (Zettel zid title tags day, doc)

-- | Like `parseZettel` but operates on multiple files.
parseZettels ::
  [(FilePath, Text)] ->
  ( [PandocZettel],
    -- | List of zettel files that cannot be parsed.
    Map ZettelID Text
  )
parseZettels fs =
  let res = flip mapMaybe fs $ \(f, s) ->
        case getZettelID f of
          Nothing -> Nothing
          Just zid ->
            Just $ first (zid,) $ parseZettel zid s
      errors = filterLeft res
      zs = filterRight res
   in (zs, Map.fromList errors)
