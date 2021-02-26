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

import qualified Data.Aeson as Aeson
import Data.Dependent.Map (DMap)
import Data.Tagged (Tagged (..))
import qualified Data.Text as T
import Data.YAML.ToJSON ()
import Neuron.Markdown
import Neuron.Zettelkasten.ID (Slug, ZettelID (unZettelID))
import Neuron.Zettelkasten.Zettel
import qualified Neuron.Zettelkasten.Zettel.Meta as Meta
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
    meta :: Maybe Meta.Meta <- case parseMeta metadata of
      Just (Aeson.Error e) -> Left $ Tagged $ "Bad metadata: " <> toText e
      Just (Aeson.Success v) -> pure (Just v)
      Nothing -> pure Nothing
    let -- Determine zettel title
        (title, titleInBody) = case Meta.title =<< meta of
          Just tit -> (tit, False)
          Nothing -> fromMaybe (unZettelID zid, False) $ do
            (,True) . P.plainify . snd <$> P.getH1 doc
        -- Determine other metadata
        date = Meta.date =<< meta
        slug = fromMaybe (mkDefaultSlug $ unZettelID zid) $ Meta.slug =<< meta
        unlisted = Just True == (Meta.unlisted =<< meta)
    pure $ Right $ Zettel zid metadata slug fn title titleInBody date unlisted doc pluginData
  where
    parseMeta m = do
      guard $ m /= Aeson.Null
      pure $ Aeson.fromJSON @Meta.Meta m
    unparseableZettel err =
      let slug = mkDefaultSlug $ unZettelID zid
       in Left $ Zettel zid Aeson.Null slug fn "Unknown" False Nothing False (s, err) pluginData
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
