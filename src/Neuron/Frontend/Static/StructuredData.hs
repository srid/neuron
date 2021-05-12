{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Frontend.Static.StructuredData
  ( renderStructuredData,
  )
where

import qualified Data.Dependent.Map as DMap
import Data.Some (Some (..))
import Data.Structured.OpenGraph
  ( Article (Article),
    OGType (..),
    OpenGraph (..),
  )
import Data.Structured.OpenGraph.Render (renderOpenGraph)
import Data.TagTree (unTag)
import qualified Data.Text as T
import Neuron.Frontend.Route (Route (..))
import qualified Neuron.Frontend.Route as R
import Neuron.Frontend.Route.Data.Types (zettelDataZettel)
import qualified Neuron.Frontend.Route.Data.Types as R
import qualified Neuron.Plugin as Plugin
import qualified Neuron.Plugin.Plugins.Tags as Tags
import Neuron.Zettelkasten.ID (unZettelID)
import Neuron.Zettelkasten.Zettel
  ( PluginZettelData (Links),
    Zettel,
    ZettelT (..),
    sansContent,
  )
import Reflex.Dom.Core
import Relude
import Text.Pandoc.Definition (Inline (Image), Pandoc (..))
import Text.Pandoc.Util (getFirstParagraphText, plainify)
import qualified Text.Pandoc.Walk as W
import qualified Text.URI as URI

renderStructuredData :: DomBuilder t m => R.RouteConfig t m -> Route a -> a -> m ()
renderStructuredData routeCfg route val = do
  renderOpenGraph $ routeOpenGraph routeCfg val route
  case route of
    R.Route_Zettel zslug -> do
      let z :: Zettel = sansContent $ R.zettelDataZettel $ snd val
          zid = zettelID z
          tags = Tags.getZettelTags z
      elAttr "meta" ("property" =: "neuron:zettel-id" <> "content" =: unZettelID zid) blank
      elAttr "meta" ("property" =: "neuron:zettel-slug" <> "content" =: zslug) blank
      forM_ tags $ \tag -> 
        elAttr "meta" ("property" =: "neuron:zettel-tag" <> "content" =: unTag tag) blank
      forM_ (DMap.toList (R.zettelDataPlugin (snd val))) $
        Plugin.renderZettelHead routeCfg val
    _ -> blank

routeOpenGraph :: R.RouteConfig t m -> a -> Route a -> OpenGraph
routeOpenGraph routeCfg v r =
  OpenGraph
    { _openGraph_title = R.routeTitle' v r,
      _openGraph_siteName = R.siteDataSiteTitle (R.routeSiteData v r),
      _openGraph_description = case r of
        Route_Impulse -> Just "Impulse"
        Route_Zettel _slug -> do
          let zData = snd v
          doc <- getPandocDoc $ R.zettelDataZettel zData
          para <- getFirstParagraphText doc
          let paraText = plainify para
          pure $ T.take 300 paraText,
      _openGraph_author = R.siteDataSiteAuthor (R.routeSiteData v r),
      _openGraph_type = case r of
        Route_Zettel _ -> Just $ OGType_Article (Article Nothing Nothing Nothing Nothing mempty)
        _ -> Just OGType_Website,
      _openGraph_image = case r of
        Route_Zettel _ -> do
          doc <- getPandocDoc (R.zettelDataZettel $ snd v)
          image <- URI.mkURI =<< getFirstImg doc
          baseUrl <- R.siteDataSiteBaseUrl (fst v)
          URI.relativeTo image baseUrl
        _ -> Nothing,
      _openGraph_url = do
        baseUrl <- R.siteDataSiteBaseUrl (R.routeSiteData v r)
        let relUrl = R.routeConfigRouteURL routeCfg (Some r)
        pure $ R.routeUri baseUrl relUrl
    }
  where
    getPandocDoc = either (const Nothing) (Just . zettelContent)
    getFirstImg ::
      Pandoc ->
      -- | Relative URL path to the image
      Maybe Text
    getFirstImg (Pandoc _ bs) = listToMaybe $
      flip W.query bs $ \case
        Image _ _ (url, _) -> [toText url]
        _ -> []
