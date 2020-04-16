{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Zettel where

import Data.Aeson
import Development.Shake (Action)
import Neuron.Zettelkasten.ID
import qualified Neuron.Zettelkasten.Markdown.Meta as Meta
import Neuron.Zettelkasten.Tag
import Relude hiding (show)
import qualified Rib.Parser.MMark as MMark
import Text.MMark (MMark)
import Text.Show (Show (show))

data Zettel = Zettel
  { zettelID :: ZettelID,
    zettelTitle :: Text,
    zettelTags :: [Tag],
    zettelContent :: MMark
  }

instance Eq Zettel where
  (==) = (==) `on` zettelID

instance Ord Zettel where
  compare = compare `on` zettelID

instance Show Zettel where
  show Zettel {..} = "Zettel:" <> show zettelID

-- TODO: Use generic deriving use field label modifier.
instance ToJSON Zettel where
  toJSON Zettel {..} =
    object
      [ "id" .= toJSON zettelID,
        "title" .= zettelTitle,
        "tags" .= toJSON zettelTags
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
  pure $ Zettel zid title tags doc

matchesTagPattern :: TagPattern -> Zettel -> Bool
matchesTagPattern pat Zettel {..} =
  isJust $ find (tagMatch pat) zettelTags

hasTag :: Tag -> Zettel -> Bool
hasTag tag zettel = matchesTagPattern (tagLiteral tag) zettel
