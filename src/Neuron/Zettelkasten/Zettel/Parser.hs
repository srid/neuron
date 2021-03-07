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
import Neuron.Frontend.Theme (titleH1Id)
import Neuron.Markdown (ZettelParser, lookupZettelMeta)
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
    let (tit, doc') = consolidateTitle metadata doc
        -- Compute final values from user (and post-plugin) metadata
        slug = fromMaybe (mkDefaultSlug $ unZettelID zid) $ lookupZettelMeta "slug" metadata
        date = lookupZettelMeta "date" metadata
    pure $ Right $ Zettel zid metadata slug date fn tit doc' (Just pluginData)
  where
    unparseableZettel err =
      let slug = mkDefaultSlug $ unZettelID zid
       in Left $ Zettel zid def slug Nothing fn "Unknown" (s, err) (Just pluginData)
    -- We keep the default slug as close to zettel ID is possible. Spaces (and
    -- colons) are replaced with underscore for legibility.
    mkDefaultSlug :: Text -> Slug
    mkDefaultSlug ss =
      foldl' (\s' x -> T.replace x "_" s') ss (charsDisallowedInURL <> [" "])
    charsDisallowedInURL :: [Text]
    charsDisallowedInURL =
      [":"]
    -- Determine the effective title of the zettel, ensuring that the same exists in the document.
    consolidateTitle metadata doc =
      case lookupZettelMeta "title" metadata of
        Just tit ->
          (tit, P.setTitleH1 titleH1Id tit doc)
        Nothing ->
          P.ensureTitleH1 titleH1Id (unZettelID zid) doc

-- | Like `parseZettel` but operates on multiple files.
parseZettels ::
  ZettelParser ->
  [(ZettelID, (FilePath, (Text, DMap PluginZettelData Identity)))] ->
  [ZettelC]
parseZettels p files =
  flip fmap files $ \(zid, (path, (s, pluginData))) ->
    parseZettel p path zid s pluginData
