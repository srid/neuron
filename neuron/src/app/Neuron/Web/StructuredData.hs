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
import qualified Data.Text as T
import Data.Time.ISO8601 (formatISO8601)
import Neuron.Config.Type
import Neuron.Web.Generate.Route (routeUri)
import Neuron.Web.Route
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.Graph (ZettelGraph)
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.Zettel
import Reflex.Dom.Core hiding ((&))
import Relude
import Rib.Extra.OpenGraph
import qualified Rib.Parser.Pandoc as Pandoc
import Text.Pandoc (runPure, writePlain)
import Text.Pandoc.Definition (Block (Plain), Inline, Pandoc (..))
import Text.Pandoc.Util (getFirstParagraphText)
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
          image <- URI.mkURI =<< Pandoc.getFirstImg doc
          baseUrl <- URI.mkURI =<< siteBaseUrl
          URI.relativeTo image baseUrl
        _ -> Nothing,
      _openGraph_url = do
        baseUrl <- siteBaseUrl
        pure $ routeUri baseUrl r
    }
  where
    getPandocDoc = either (const Nothing) (Just . zettelContent)

renderOpenGraph :: forall t m. DomBuilder t m => OpenGraph -> m ()
renderOpenGraph OpenGraph {..} = do
  meta' "author" `mapM_` _openGraph_author
  meta' "description" `mapM_` _openGraph_description
  requireAbsolute "OGP URL" (\ourl -> elAttr "link" ("rel" =: "canonical" <> "href" =: ourl) blank) `mapM_` _openGraph_url
  metaOg "title" _openGraph_title
  metaOg "site_name" _openGraph_siteName
  whenJust _openGraph_type $ \case
    OGType_Article (Article {..}) -> do
      metaOg "type" "article"
      metaOg "article:section" `mapM_` _article_section
      metaOgTime "article:modified_time" `mapM_` _article_modifiedTime
      metaOgTime "article:published_time" `mapM_` _article_publishedTime
      metaOgTime "article:expiration_time" `mapM_` _article_expirationTime
      metaOg "article:tag" `mapM_` _article_tag
    OGType_Website -> do
      metaOg "type" "website"
  requireAbsolute "OGP image URL" (metaOg "image") `mapM_` _openGraph_image
  where
    meta' k v =
      elAttr "meta" ("name" =: k <> "content" =: v) blank
    metaOg k v =
      elAttr "meta" ("property" =: ("og:" <> k) <> "content" =: v) blank
    metaOgTime k t =
      metaOg k $ toText $ formatISO8601 t
    requireAbsolute :: Text -> (Text -> m ()) -> URI.URI -> m ()
    requireAbsolute description f uri' =
      if isJust (URI.uriScheme uri')
        then f $ URI.render uri'
        else error $ description <> " must be absolute. this URI is not: " <> URI.render uri'

renderPandocAsText :: [Inline] -> Maybe Text
renderPandocAsText =
  either (const Nothing) Just . runPure . writePlain def . Pandoc mempty . pure . Plain
