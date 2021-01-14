{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Zettel.Parser where

import Data.Dependent.Map (DMap)
import qualified Data.Set as Set
import Data.TagTree (unTag)
import qualified Data.Text as T
import Neuron.Markdown (parseMarkdown)
import Neuron.Zettelkasten.ID (Slug, ZettelID (unZettelID))
import Neuron.Zettelkasten.Zettel
import qualified Neuron.Zettelkasten.Zettel.Meta as Meta
import Relude
import qualified Text.Pandoc.Util as P

parseZettel ::
  FilePath ->
  ZettelID ->
  Text ->
  DMap PluginZettelData Identity ->
  ZettelC
parseZettel fn zid s pluginData = do
  case parseMarkdown fn s of
    Left parseErr ->
      let slug = mkDefaultSlug $ unZettelID zid
       in Left $ Zettel zid slug fn "Unknown" False Set.empty Nothing False (s, parseErr) pluginData
    Right (meta, doc) ->
      let -- Determine zettel title
          (title, titleInBody) = case Meta.title =<< meta of
            Just tit -> (tit, False)
            Nothing -> fromMaybe (unZettelID zid, False) $ do
              (,True) . P.plainify . snd <$> P.getH1 doc
          -- Determine zettel tags
          -- TODO: Move to Tags plugin!
          metaTags = fromMaybe [] $ Meta.tags =<< meta
          tags = Set.fromList metaTags
          -- Determine other metadata
          date = Meta.date =<< meta
          slug = fromMaybe (mkDefaultSlug $ unZettelID zid) $ Meta.slug =<< meta
          unlisted = Just True == (Meta.unlisted =<< meta)
       in Right $
            Zettel zid slug fn title titleInBody tags date unlisted doc pluginData
  where
    _dirFolgezettelMarkdown (unTag -> tag) =
      "\n\n" <> "[[[z:zettels?tag=" <> tag <> "/*]]]"
    mkDefaultSlug :: Text -> Slug
    mkDefaultSlug ss =
      foldl' (\s' x -> T.replace x "_" s') ss (charsDisallowedInURL <> [" "])
    charsDisallowedInURL :: [Text]
    charsDisallowedInURL =
      [":"]

-- | Like `parseZettel` but operates on multiple files.
parseZettels ::
  [(ZettelID, (FilePath, (Text, DMap PluginZettelData Identity)))] ->
  [ZettelC]
parseZettels files =
  flip fmap files $ \(zid, (path, (s, pluginData))) ->
    parseZettel path zid s pluginData
