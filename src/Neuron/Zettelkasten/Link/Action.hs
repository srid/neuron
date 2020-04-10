{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Special Zettel links in Markdown
module Neuron.Zettelkasten.Link.Action where

import Control.Foldl (Fold (..))
import qualified Data.Set as Set
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Query
import Neuron.Zettelkasten.Store
import Neuron.Zettelkasten.Zettel
import Relude
import Text.MMark (MMark, runScanner)
import qualified Text.MMark.Extension as Ext
import Text.MMark.Extension (Inline (..))
import qualified Text.URI as URI

data LinkTheme
  = LinkTheme_Default
  | LinkTheme_Simple
  | LinkTheme_WithDate
  deriving (Eq, Show, Ord)

data LinkAction
  = LinkAction_ConnectZettel Connection ZettelID
  | -- | Render a list (or should it be tree?) of links to queries zettels
    LinkAction_QueryZettels Connection LinkTheme [Query]
  deriving (Eq, Show)

linkActionFromLink :: MarkdownLink -> Maybe LinkAction
linkActionFromLink MarkdownLink {markdownLinkUri=uri,markdownLinkText=text} =
  -- NOTE: We should probably drop the 'cf' variants in favour of specifying
  -- the connection type as a query param or something.
  case fmap URI.unRText (URI.uriScheme uri) of
    Just "z" ->
      -- The inner link text is supposed to be the zettel ID
      let zid = parseZettelID text
       in Just $ LinkAction_ConnectZettel Folgezettel zid
    Just "zcf" ->
      -- The inner link text is supposed to be the zettel ID
      let zid = parseZettelID text
       in Just $ LinkAction_ConnectZettel OrdinaryConnection zid
    Just "zquery" ->
      Just $ LinkAction_QueryZettels Folgezettel (fromMaybe LinkTheme_Default $ linkThemeFromUri uri) (queryFromUri uri)
    Just "zcfquery" ->
      Just $ LinkAction_QueryZettels OrdinaryConnection (fromMaybe LinkTheme_Default $ linkThemeFromUri uri) (queryFromUri uri)
    _ ->
      Nothing

queryFromUri :: URI.URI -> [Query]
queryFromUri uri =
  flip mapMaybe (URI.uriQuery uri) $ \case
    URI.QueryParam (URI.unRText -> key) (URI.unRText -> val) ->
      case key of
        "tag" -> Just $ ByTag val
        _ -> Nothing
    _ -> Nothing

linkThemeFromUri :: URI.URI -> Maybe LinkTheme
linkThemeFromUri uri =
  listToMaybe $ flip mapMaybe (URI.uriQuery uri) $ \case
    URI.QueryFlag _ -> Nothing
    URI.QueryParam (URI.unRText -> key) (URI.unRText -> val) ->
      case key of
        "linkTheme" ->
          case val of
            "default" -> Just LinkTheme_Default
            "simple" -> Just LinkTheme_Simple
            "withDate" -> Just LinkTheme_WithDate
            _ -> error $ "Unknown link theme: " <> val
        _ -> Nothing

data MarkdownLink
  = MarkdownLink
      { markdownLinkText :: Text,
        markdownLinkUri :: URI.URI
      }
  deriving (Eq, Ord)

linkActionConnections :: ZettelStore -> MarkdownLink -> [ZettelConnection]
linkActionConnections store link =
  case linkActionFromLink link of
    Just (LinkAction_ConnectZettel conn zid) ->
      [(conn, zid)]
    Just (LinkAction_QueryZettels conn _linkTheme q) ->
      (conn,) . zettelID <$> runQuery store q
    Nothing ->
      []

-- | Extract all links from the Markdown document
extractLinks :: MMark -> [MarkdownLink]
extractLinks = Set.toList . Set.fromList . flip runScanner (Fold go [] id)
  where
    go acc blk = acc <> concat (fmap f (relevantInlines blk))
    f = \case
      Link inner uri _title ->
        [MarkdownLink (Ext.asPlainText inner) uri]
      _ ->
        []
    relevantInlines = \case
      Ext.Naked xs -> toList xs
      Ext.Paragraph xs -> toList xs
      Ext.OrderedList _ xs -> concat $ concat $ fmap (fmap relevantInlines) xs
      Ext.UnorderedList xs -> concat $ concat $ fmap (fmap relevantInlines) xs
      _ -> []
