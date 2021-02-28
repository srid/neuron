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

module Neuron.Plugin.Plugins.Feed (plugin, routePluginData, renderZettelHead, writeFeed) where

import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import qualified Data.Map.Strict as Map
import Data.Some (Some (Some))
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
import qualified Text.Pandoc.Util as Pandoc
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
            fmapMaybe (mkFeedItem baseUrl) $
              -- Get all downlinks of this zettel
              downlinksWithContent zettels z g
      title = zettelTitle z
   in FeedData title (zettelSlug z) baseUrl permaLink entries
  where
    downlinksWithContent zettels zettel graph =
      fforMaybe (G.downlinks zettel graph) $ \dz ->
        Map.lookup (zettelID dz) zettels
    mkFeedItem baseUrl Zettel {..} = do
      feedItemDate <- zettelDate
      let feedItemSlug = zettelSlug
          feedItemZettelContent = zettelContent
          feedItemTitle = zettelTitle
          feedItemAuthor = siteDataSiteAuthor siteData
          feedItemZettelID = zettelID
          feedItemUrl = R.routeUri baseUrl $ R.routeConfigRouteURL routeCfg (Some $ R.Route_Zettel zettelSlug)
      pure FeedItem {..}

renderZettelHead :: DomBuilder t m => (SiteData, ZettelData) -> FeedData -> m ()
renderZettelHead v FeedData {..} = do
  let feedUrl = URI.render $ R.routeUri feedDataBaseUri $ toText $ feedPath (either zettelSlug zettelSlug $ zettelDataZettel $ snd v)
  elAttr
    "link"
    ( "rel" =: "alternate"
        <> "type" =: "application/atom+xml"
        <> "title" =: ("Atom feed for " <> feedDataTitle)
        <> "href" =: feedUrl
    )
    blank

writeFeed ::
  MonadIO m =>
  (ZettelData -> Pandoc -> IO ByteString) ->
  DMap Route Identity ->
  Slug ->
  FeedData ->
  m (Either Text [(Text, FilePath, LText)])
writeFeed elZettel allRoutes slug FeedData {..} = do
  case nonEmpty feedDataEntries of
    Nothing -> do
      pure $ Left $ toText $ "No entries available to generate " <> feedPath feedDataSlug
    Just items -> do
      entries <-
        catMaybes . toList
          <$> traverse (mkFeedEntry elZettel allRoutes) items
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
  (ZettelData -> Pandoc -> IO ByteString) ->
  DMap Route Identity ->
  FeedItem ->
  m1 (Maybe Atom.Entry)
mkFeedEntry elZettel allRoutes FeedItem {..} = do
  case DMap.lookup (R.Route_Zettel feedItemSlug) allRoutes of
    Nothing ->
      -- This won't be reached.
      pure Nothing
    Just (Identity (_siteData, zData)) -> do
      html <- liftIO $ elZettel zData $ Pandoc.deleteTitleH1 feedItemZettelContent
      pure $
        Just $
          ( Atom.nullEntry
              (URI.render feedItemUrl)
              (Atom.TextString feedItemTitle)
              (DMT.formatDateMayTime feedItemDate)
          )
            { Atom.entryContent = Just (Atom.HTMLContent $ decodeUtf8 html),
              Atom.entryLinks = one $ Atom.nullLink (URI.render feedItemUrl)
            }
