{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Zettel ID
module Neuron.Zettelkasten.ID
  ( ZettelID (..),
    Connection (..),
    ZettelConnection,
    zettelIDDate,
    connectionScheme,
    parseZettelID,
    mkZettelID,
    zettelIDsFromMMark,
  )
where

import Control.Foldl (Fold (..))
import qualified Data.Set as Set
import qualified Data.Text as T
import Data.Time.Calendar
import Data.Time.Format
import Lucid
import Path
import Relude
import Text.MMark (MMark, runScanner)
import Text.MMark.Extension (Inline (..))
import qualified Text.MMark.Extension as Ext
import qualified Text.URI as URI
import Text.URI.QQ (scheme)

-- Short Zettel ID encoding datetime.
--
-- Based on https://old.reddit.com/r/Zettelkasten/comments/fa09zw/shorter_zettel_ids/
newtype ZettelID = ZettelID {unZettelID :: Text}
  deriving (Eq, Show, Ord)

-- TODO: sync/DRY with zettelNextIdForToday
zettelIDDate :: ZettelID -> Day
zettelIDDate =
  parseTimeOrError False defaultTimeLocale "%y%W%a"
    . toString
    . uncurry mappend
    . second (dayFromIndex . readMaybe . toString)
    . (T.dropEnd 1 &&& T.takeEnd 1)
    . T.dropEnd 2
    . unZettelID
  where
    dayFromIndex :: Maybe Int -> Text
    dayFromIndex = \case
      Just n ->
        case n of
          1 -> "Mon"
          2 -> "Tue"
          3 -> "Wed"
          4 -> "Thu"
          5 -> "Fri"
          6 -> "Sat"
          7 -> "Sun"
          _ -> error "> 7"
      Nothing ->
        error "Bad day"

type ZettelConnection = (Connection, ZettelID)

-- | Represent the connection between zettels
data Connection
  = -- | A folgezettel points to a zettel that is conceptually a part of the
    -- parent zettel.
    Folgezettel
  | -- | Any other ordinary connection (eg: "See also")
    OrdinaryConnection
  deriving (Eq, Show, Enum, Bounded)

-- TODO: avoid DRY with Link.hs
connectionScheme :: Connection -> URI.RText l
connectionScheme = \case
  Folgezettel -> [scheme|z|]
  OrdinaryConnection -> [scheme|zcf|]

instance ToHtml ZettelID where
  toHtmlRaw = toHtml
  toHtml = toHtml . unZettelID

-- TODO: Actually parse and validate
parseZettelID :: Text -> ZettelID
parseZettelID = ZettelID

-- | Extract ZettelID from the zettel's filename or path.
mkZettelID :: Path Rel File -> ZettelID
mkZettelID fp = either (error . toText . displayException) id $ do
  (name, _) <- splitExtension $ filename fp
  pure $ ZettelID $ toText $ toFilePath name

-- | TODO: This should support multiple protocols
mkZettelIDFromMarkdownLink :: MarkdownLink -> Maybe (Connection, ZettelID)
mkZettelIDFromMarkdownLink MarkdownLink {..} =
  listToMaybe $ flip mapMaybe connections $ \c ->
    if URI.uriScheme markdownLinkUri == Just (connectionScheme c)
      then Just (c, ZettelID markdownLinkText)
      else Nothing
  where
    connections :: [Connection]
    connections = [minBound .. maxBound]

zettelIDsFromMMark :: MMark -> [(Connection, ZettelID)]
zettelIDsFromMMark = mapMaybe mkZettelIDFromMarkdownLink . extractLinks

-- Utility

data MarkdownLink
  = MarkdownLink
      { markdownLinkText :: Text,
        markdownLinkUri :: URI.URI
      }
  deriving (Eq, Ord)

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
