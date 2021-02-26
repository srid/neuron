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
        -- Compute final values from user (and post-plugin) metadata
        slug = fromMaybe (mkDefaultSlug $ unZettelID zid) $ lookupZettelMeta "slug" metadata
        date = lookupZettelMeta "date" metadata
    pure $ Right $ Zettel zid metadata slug date fn titleFinal titleInBody doc (Just pluginData)
  where
    unparseableZettel err =
      let slug = mkDefaultSlug $ unZettelID zid
       in Left $ Zettel zid def slug Nothing fn "Unknown" False (s, err) (Just pluginData)
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
