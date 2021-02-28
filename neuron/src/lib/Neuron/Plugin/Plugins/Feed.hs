{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Plugin.Plugins.Feed (plugin, routePluginData, writeFeed) where

import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import qualified Data.Map.Strict as Map
import Data.Some
import qualified Data.Time.DateMayTime as DMT
import GHC.Natural (naturalToInt)
import Neuron.Frontend.Route (Route)
import qualified Neuron.Frontend.Route as R
import Neuron.Frontend.Route.Data.Types
import Neuron.Markdown (lookupZettelMeta)
import Neuron.Plugin.Type (Plugin (..))
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.ID (Slug)
import Neuron.Zettelkasten.Zettel
import Reflex.Dom.Core
import Relude
import qualified Text.Atom.Feed as Atom
import qualified Text.Atom.Feed.Export as Export (textFeed)
import Text.Pandoc.Definition (Pandoc)
import qualified Text.URI as URI

plugin :: Plugin FeedData
plugin =
  def
    { _plugin_afterZettelParse = bimap enable enable,
      _plugin_afterRouteWrite = writeFeed
    }

enable :: ZettelT c -> ZettelT c
enable z =
  case lookupZettelMeta @FeedMeta "feed" (zettelMeta z) of
    Nothing -> z
    Just meta ->
      setPluginData Feed meta z

routePluginData :: R.RouteConfig t m -> SiteData -> [ZettelC] -> ZettelGraph -> ZettelC -> FeedMeta -> FeedData
routePluginData routeCfg siteData zs g (sansContent -> z) FeedMeta {..} =
  let zettels =
        Map.fromList $
          rights zs <&> \z'@Zettel {..} ->
            (zettelID, z')
      baseUrl = fromMaybe (error "You must specify a base url in neuron.dhall to use feed plugin") $ siteDataSiteBaseUrl siteData
      permaLink = R.routeUri baseUrl $ R.routeConfigRouteURL routeCfg (Some $ R.Route_Zettel $ zettelSlug z)
      entries =
        -- Limit to user-chosen count
        take (naturalToInt feedmetaCount) $
          -- In reverse chronologial order
          sortOn (Down . feedItemDate) $
            -- Only those with date assigned
            fmapMaybe mkFeedItem $
              -- Get all downlinks of this zettel
              downlinksWithContent zettels z g
      title = zettelTitle z
   in FeedData title (zettelSlug z) baseUrl permaLink entries
  where
    downlinksWithContent zettels zettel graph =
      fforMaybe (G.downlinks zettel graph) $ \dz ->
        Map.lookup (zettelID dz) zettels
    mkFeedItem Zettel {..} = do
      feedItemDate <- zettelDate
      let feedItemSlug = zettelSlug
          feedItemZettelContent = zettelContent
          feedItemTitle = zettelTitle
          feedItemAuthor = siteDataSiteAuthor siteData
          feedItemZettelID = zettelID
      pure FeedItem {..}

writeFeed ::
  MonadIO m =>
  R.RouteConfig t m1 ->
  (ZettelData -> Pandoc -> IO ByteString) ->
  DMap Route Identity ->
  Slug ->
  FeedData ->
  m (Either Text [(Text, FilePath, LText)])
writeFeed routeCfg elZettel allRoutes slug FeedData {..} = do
  case nonEmpty feedDataEntries of
    Nothing -> do
      pure $ Left $ toText $ "No entries available to generate " <> feedPath feedDataSlug
    Just items -> do
      entries <-
        catMaybes . toList
          <$> traverse (mkFeedEntry routeCfg elZettel allRoutes feedDataBaseUri) items
      -- Last updated is automatically determined from the recent zettel's date
      let lastUpdated = feedItemDate $ head items
          feed =
            ( Atom.nullFeed
                (URI.render feedDataUrl)
                (Atom.TextString feedDataTitle)
                (DMT.formatDateMayTime lastUpdated)
            )
              { Atom.feedEntries = entries
              }
          feedText = fromMaybe (error "Feed malformed?") $ Export.textFeed feed
      pure $ Right $ one ("feed", feedPath slug, feedText)

feedPath :: Slug -> String
feedPath slug =
  toString slug <> ".xml"

mkFeedEntry ::
  MonadIO m1 =>
  R.RouteConfig t m ->
  (ZettelData -> Pandoc -> IO ByteString) ->
  DMap Route Identity ->
  URI.URI ->
  FeedItem ->
  m1 (Maybe Atom.Entry)
mkFeedEntry routeCfg elZettel allRoutes baseUrl FeedItem {..} = do
  case DMap.lookup (R.Route_Zettel feedItemSlug) allRoutes of
    Nothing ->
      -- This won't be reached.
      pure Nothing
    Just (Identity (_siteData, zData)) -> do
      let url = URI.render $ R.routeUri baseUrl $ R.routeConfigRouteURL routeCfg (Some $ R.Route_Zettel $ either zettelSlug zettelSlug $ zettelDataZettel zData)
      html <- liftIO $ elZettel zData feedItemZettelContent
      pure $
        Just $
          ( Atom.nullEntry
              url
              (Atom.TextString feedItemTitle)
              (DMT.formatDateMayTime feedItemDate)
          )
            { Atom.entryContent = Just (Atom.HTMLContent $ decodeUtf8 html),
              Atom.entryLinks = one $ Atom.nullLink feedItemSlug
            }