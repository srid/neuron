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
import qualified Neuron.Zettelkasten.Meta as Meta
import Relude hiding (show)
import qualified Rib.Parser.MMark as MMark
import Text.MMark (MMark)
import Text.Show (Show (show))

data Zettel content = Zettel
  { zettelID :: ZettelID,
    zettelTitle :: Text,
    zettelTags :: [Text],
    zettelContent :: content
  }
  deriving (Functor)

instance Eq (Zettel c) where
  (==) = (==) `on` zettelID

instance Ord (Zettel c) where
  compare = compare `on` zettelID

instance Show (Zettel c) where
  show Zettel {..} = "Zettel:" <> show zettelID

-- TODO: Use generic deriving use field label modifier.
instance ToJSON (Zettel ()) where
  toJSON Zettel {..} =
    object
      [ "id" .= toJSON zettelID,
        "title" .= zettelTitle,
        "tags" .= zettelTags
      ]

-- | Load a zettel from a file.
mkZettelFromPath :: FilePath -> Action (Zettel MMark)
mkZettelFromPath path = do
  -- Extensions are computed and applied during rendering, not here.
  let noExts = []
  doc <- MMark.parseWith noExts path
  let zid = mkZettelID path
      meta = Meta.getMeta doc
      title = maybe (toText $ "No title for " <> path) Meta.title meta
      tags = fromMaybe [] $ Meta.tags =<< meta
  pure $ Zettel zid title tags doc

hasTag :: forall c. Text -> Zettel c -> Bool
hasTag t Zettel {..} =
  isJust $ find (== t) zettelTags
