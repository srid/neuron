{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.MMark.MarkdownLink
  ( MarkdownLink (..),
    extractLinks,
  )
where

import Control.Foldl (Fold (..))
import qualified Data.Set as Set
import Relude
import Text.MMark (MMark, runScanner)
import Text.MMark.Extension (Inline (..))
import qualified Text.MMark.Extension as Ext
import qualified Text.URI as URI

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
      Ext.Blockquote xs -> concatMap relevantInlines xs
      Ext.OrderedList _ xs -> concat $ concatMap (fmap relevantInlines) xs
      Ext.UnorderedList xs -> concat $ concatMap (fmap relevantInlines) xs
      _ -> []
