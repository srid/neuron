{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Zettel.Parser where

import Data.Default (Default (def))
import Data.Dependent.Map (DMap)
import qualified Data.Text as T
import Data.YAML.ToJSON ()
import Neuron.Markdown
import Neuron.Zettelkasten.ID (Slug, ZettelID (unZettelID))
import Neuron.Zettelkasten.Zettel
import Relude
import qualified Text.Pandoc.Util as P

parseZettel ::
  ZettelParser ->
  FilePath ->
  ZettelID ->
  Text ->
  DMap PluginZettelData Identity ->
  ZettelC
parseZettel parser fn zid s pluginData =
  either unparseableZettel id $ do
    (metadata, doc) <- parser fn s
    let -- Determine zettel title
        (titleFinal, titleInBody) = case lookupZettelMeta "title" metadata of
          Just tit -> (tit, False)
          Nothing -> fromMaybe (unZettelID zid, False) $ do
            (,True) . P.plainify . snd <$> P.getH1 doc
        -- Determine other metadata
        slugFinal = fromMaybe (mkDefaultSlug $ unZettelID zid) $ lookupZettelMeta "slug" metadata
        unlistedFinal = Just True == lookupZettelMeta "unlisted" metadata
    pure $ Right $ Zettel zid metadata slugFinal fn titleFinal titleInBody (lookupZettelMeta "date" metadata) unlistedFinal doc pluginData
  where
    unparseableZettel err =
      let slugFinal = mkDefaultSlug $ unZettelID zid
       in Left $ Zettel zid def slugFinal fn "Unknown" False Nothing False (s, err) pluginData
    -- We keep the default slug as close to zettel ID is possible. Spaces (and
    -- colons) are replaced with underscore for legibility.
    mkDefaultSlug :: Text -> Slug
    mkDefaultSlug ss =
      foldl' (\s' x -> T.replace x "_" s') ss (charsDisallowedInURL <> [" "])
    charsDisallowedInURL :: [Text]
    charsDisallowedInURL =
      [":"]

-- | Like `parseZettel` but operates on multiple files.
parseZettels ::
  ZettelParser ->
  [(ZettelID, (FilePath, (Text, DMap PluginZettelData Identity)))] ->
  [ZettelC]
parseZettels p files =
  flip fmap files $ \(zid, (path, (s, pluginData))) ->
    parseZettel p path zid s pluginData
