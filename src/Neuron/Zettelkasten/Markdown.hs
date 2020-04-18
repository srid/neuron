{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Markdown
  ( neuronMMarkExts,
    MarkdownLink (..),
    extractLinks,
  )
where

import Control.Foldl (Fold (..))
import qualified Data.Set as Set
import Neuron.Config (Config (..))
import Neuron.Zettelkasten.Markdown.Extension (setTableClass)
import Relude
import Text.MMark (MMark, runScanner)
import qualified Text.MMark as MMark
import Text.MMark.Extension (Inline (..))
import qualified Text.MMark.Extension as Ext
import qualified Text.MMark.Extension.Common as Ext
import qualified Text.URI as URI

neuronMMarkExts :: Config -> [MMark.Extension]
neuronMMarkExts Config {..} =
  defaultExts
    <> bool [] [Ext.mathJax (Just '$')] mathJaxSupport

defaultExts :: [MMark.Extension]
defaultExts =
  [ Ext.fontAwesome,
    Ext.footnotes,
    Ext.kbd,
    Ext.linkTarget,
    Ext.punctuationPrettifier,
    Ext.skylighting,
    setTableClass "ui celled table"
  ]

data MarkdownLink = MarkdownLink
  { markdownLinkText :: Text,
    markdownLinkUri :: URI.URI
  }
  deriving (Eq, Ord)

-- | Extract all links from the Markdown document
extractLinks :: MMark -> [MarkdownLink]
extractLinks = Set.toList . Set.fromList . flip runScanner (Fold go [] id)
  where
    go acc blk = acc <> concatMap f (relevantInlines blk)
    f = \case
      Link inner uri _title ->
        [MarkdownLink (Ext.asPlainText inner) uri]
      _ ->
        []
    relevantInlines = \case
      Ext.Naked xs -> toList xs
      Ext.Paragraph xs -> toList xs
      Ext.OrderedList _ xs -> concat $ concatMap (fmap relevantInlines) xs
      Ext.UnorderedList xs -> concat $ concatMap (fmap relevantInlines) xs
      _ -> []
