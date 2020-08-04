{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Web.StructuredData
  ( renderStructuredData,
  )
where

import Data.Structured.Breadcrumb (Breadcrumb)
import qualified Data.Structured.Breadcrumb as Breadcrumb
import Data.Structured.OpenGraph
import Data.Structured.OpenGraph.Render (renderOpenGraph)
import qualified Data.Text as T
import Neuron.Config.Type
import Neuron.Web.Generate.Route (routeUri)
import Neuron.Web.Route
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.Graph (ZettelGraph)
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.Zettel
import Reflex.Dom.Core hiding ((&))
import Relude
import Text.Pandoc (runPure, writePlain)
import Text.Pandoc.Definition (Block (Plain), Inline (Image), Pandoc (..))
import Text.Pandoc.Util (getFirstParagraphText)
import Text.Pandoc.Walk (query)
import qualified Text.URI as URI

renderStructuredData :: DomBuilder t m => Config -> Route a -> (ZettelGraph, a) -> m ()
renderStructuredData config route val = do
  renderOpenGraph $ routeOpenGraph config (snd val) route
  Breadcrumb.renderBreadcrumbs $ routeStructuredData config val route

routeStructuredData :: Config -> (ZettelGraph, a) -> Route a -> [Breadcrumb]
routeStructuredData Config {..} (graph, v) = \case
  Route_Zettel _ ->
    case siteBaseUrl of
      Nothing -> []
      Just baseUrl ->
        let mkCrumb :: Zettel -> Breadcrumb.Item
            mkCrumb Zettel {..} =
              Breadcrumb.Item zettelTitle (Just $ routeUri baseUrl $ Route_Zettel zettelID)
         in Breadcrumb.fromForest $ fmap mkCrumb <$> G.backlinkForest Folgezettel (sansContent v) graph
  _ ->
    []

routeOpenGraph :: Config -> a -> Route a -> OpenGraph
routeOpenGraph Config {..} v r =
  OpenGraph
    { _openGraph_title = routeTitle' v r,
      _openGraph_siteName = siteTitle,
      _openGraph_description = case r of
        Route_Redirect _ -> Nothing
        Route_ZIndex -> Just "Zettelkasten Index"
        (Route_Search _mtag) -> Just "Search Zettelkasten"
        Route_Zettel _ -> do
          doc <- getPandocDoc v
          para <- getFirstParagraphText doc
          paraText <- renderPandocAsText para
          pure $ T.take 300 paraText,
      _openGraph_author = author,
      _openGraph_type = case r of
        Route_Zettel _ -> Just $ OGType_Article (Article Nothing Nothing Nothing Nothing mempty)
        _ -> Just OGType_Website,
      _openGraph_image = case r of
        Route_Zettel _ -> do
          doc <- getPandocDoc v
          image <- URI.mkURI =<< getFirstImg doc
          baseUrl <- URI.mkURI =<< siteBaseUrl
          URI.relativeTo image baseUrl
        _ -> Nothing,
      _openGraph_url = do
        baseUrl <- siteBaseUrl
        pure $ routeUri baseUrl r
    }
  where
    getPandocDoc = either (const Nothing) (Just . zettelContent)
    getFirstImg ::
      Pandoc ->
      -- | Relative URL path to the image
      Maybe Text
    getFirstImg (Pandoc _ bs) = listToMaybe $
      flip query bs $ \case
        Image _ _ (url, _) -> [toText url]
        _ -> []

renderPandocAsText :: [Inline] -> Maybe Text
renderPandocAsText =
  either (const Nothing) Just . runPure . writePlain def . Pandoc mempty . pure . Plain
