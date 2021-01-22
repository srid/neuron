{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Zettel.Parser where

import Data.Dependent.Map (DMap)
import qualified Data.Text as T
import qualified Data.YAML as Y
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
  (Maybe (Y.Node Y.Pos), ZettelC)
parseZettel parser fn zid s pluginData =
  either unparseableZettel id $ do
    (yamlNode, doc) <- parser fn s
    meta :: Maybe Meta.Meta <- parseYamlNode @Meta.Meta `traverse` yamlNode
    let -- Determine zettel title
        (title, titleInBody) = case Meta.title =<< meta of
          Just tit -> (tit, False)
          Nothing -> fromMaybe (unZettelID zid, False) $ do
            (,True) . P.plainify . snd <$> P.getH1 doc
        -- Determine other metadata
        date = Meta.date =<< meta
        slug = fromMaybe (mkDefaultSlug $ unZettelID zid) $ Meta.slug =<< meta
        unlisted = Just True == (Meta.unlisted =<< meta)
    pure $ (yamlNode,) $ Right $ Zettel zid slug fn title titleInBody date unlisted doc pluginData
  where
    unparseableZettel err =
      let slug = mkDefaultSlug $ unZettelID zid
       in (Nothing,) $ Left $ Zettel zid slug fn "Unknown" False Nothing False (s, err) pluginData
    mkDefaultSlug :: Text -> Slug
    mkDefaultSlug ss =
      foldl' (\s' x -> T.replace x "-" s') (T.toLower ss) (charsDisallowedInURL <> [" "])
    charsDisallowedInURL :: [Text]
    charsDisallowedInURL =
      [":"]

-- | Like `parseZettel` but operates on multiple files.
parseZettels ::
  ZettelParser ->
  [(ZettelID, (FilePath, (Text, DMap PluginZettelData Identity)))] ->
  [(Maybe (Y.Node Y.Pos), ZettelC)]
parseZettels p files =
  flip fmap files $ \(zid, (path, (s, pluginData))) ->
    parseZettel p path zid s pluginData
