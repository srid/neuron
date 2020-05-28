{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | HTML & CSS
module Neuron.Web.View
  ( renderRouteHead,
    renderRouteBody,
    style,
  )
where

import Clay hiding (Plain, id, ms, object, reverse, s, style, type_)
import qualified Clay as C
import Control.Monad.Except
import Data.Aeson ((.=), object)
import qualified Data.Aeson.Text as Aeson
import Data.Default (def)
import Data.FileEmbed (embedStringFile)
import Data.Foldable (maximum)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Structured.Breadcrumb (Breadcrumb)
import qualified Data.Structured.Breadcrumb as Breadcrumb
import Data.TagTree (Tag (..))
import Data.Tagged
import qualified Data.Text as T
import Data.Time.ISO8601 (formatISO8601)
import Data.Tree (Tree (..))
import Neuron.Config
import Neuron.Markdown (getFirstParagraphText)
import Neuron.Version (neuronVersion)
import Neuron.Web.Generate.Route (routeUri)
import qualified Neuron.Web.Query.View as QueryView
import Neuron.Web.Route
import qualified Neuron.Web.Theme as Theme
import qualified Neuron.Web.Zettel.CSS as ZettelCSS
import qualified Neuron.Web.Zettel.View as ZettelView
import Neuron.Zettelkasten.Connection
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.Graph (ZettelGraph)
import Neuron.Zettelkasten.ID (ZettelID, zettelIDSourceFileName, zettelIDText)
import Neuron.Zettelkasten.Query.Error (QueryError, showQueryError)
import Neuron.Zettelkasten.Zettel
import Reflex.Dom.Core hiding ((&))
import Reflex.Dom.Pandoc (PandocBuilder)
import Relude hiding ((&))
import Rib (routeUrlRel)
import Rib.Extra.OpenGraph
import qualified Rib.Parser.Pandoc as Pandoc
import qualified Skylighting.Format.HTML as Skylighting
import qualified Skylighting.Styles as Skylighting
import Text.Pandoc (runPure, writePlain)
import Text.Pandoc.Definition (Block (Plain), Inline, Pandoc (..))
import qualified Text.URI as URI

searchScript :: Text
searchScript = $(embedStringFile "./src-js/search.js")

renderRouteHead :: DomBuilder t m => Config -> Route a -> (ZettelGraph, a) -> m ()
renderRouteHead config route val = do
  elAttr "meta" ("http-equiv" =: "Content-Type" <> "content" =: "text/html; charset=utf-8") blank
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
  el "title" $ text $ routeTitle config (snd val) route
  elAttr "link" ("rel" =: "shortcut icon" <> "href" =: "https://raw.githubusercontent.com/srid/neuron/master/assets/logo.ico") blank
  case route of
    Route_Redirect _ ->
      blank
    Route_Search {} -> do
      forM_
        [ "https://cdn.jsdelivr.net/npm/jquery@3.5.0/dist/jquery.min.js",
          "https://cdn.jsdelivr.net/npm/semantic-ui@2.4.2/dist/semantic.min.js",
          "https://cdn.jsdelivr.net/npm/js-search@2.0.0/dist/umd/js-search.min.js"
        ]
        $ \scrpt -> do
          elAttr "script" ("src" =: scrpt) blank
    _ -> do
      renderOpenGraph $ routeOpenGraph config (snd val) route
      Breadcrumb.renderBreadcrumbs $ routeStructuredData config val route
      elAttr "style" ("type" =: "text/css") $ text $ toText $ Skylighting.styleToCss Skylighting.tango
  where
    routeStructuredData :: Config -> (ZettelGraph, a) -> Route a -> [Breadcrumb]
    routeStructuredData Config {..} (graph, v) = \case
      Route_Zettel _ ->
        case siteBaseUrl of
          Nothing -> []
          Just baseUrl ->
            let mkCrumb :: Zettel -> Breadcrumb.Item
                mkCrumb Zettel {..} =
                  Breadcrumb.Item zettelTitle (Just $ routeUri baseUrl $ Route_Zettel zettelID)
             in Breadcrumb.fromForest $ fmap mkCrumb <$> G.backlinkForest Folgezettel (fst $ unPandocZettel v) graph
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
            Route_Search -> Just "Search Zettelkasten"
            Route_Zettel _ -> do
              let PandocZettel (_, doc) = v
              para <- getFirstParagraphText doc
              paraText <- renderPandocAsText para
              pure $ T.take 300 paraText,
          _openGraph_author = author,
          _openGraph_type = case r of
            Route_Zettel _ -> Just $ OGType_Article (Article Nothing Nothing Nothing Nothing mempty)
            _ -> Just OGType_Website,
          _openGraph_image = case r of
            Route_Zettel _ -> do
              let PandocZettel (_, doc) = v
              image <- URI.mkURI =<< Pandoc.getFirstImg doc
              baseUrl <- URI.mkURI =<< siteBaseUrl
              URI.relativeTo image baseUrl
            _ -> Nothing,
          _openGraph_url = do
            baseUrl <- siteBaseUrl
            pure $ routeUri baseUrl r
        }
    routeTitle :: Config -> a -> Route a -> Text
    routeTitle Config {..} v =
      withSuffix siteTitle . routeTitle' v
      where
        withSuffix suffix x =
          if x == suffix
            then x
            else x <> " - " <> suffix
    routeTitle' :: a -> Route a -> Text
    routeTitle' v = \case
      Route_Redirect _ -> "Redirecting..."
      Route_ZIndex -> "Zettel Index"
      Route_Search -> "Search"
      Route_Zettel _ ->
        let PandocZettel (z, _) = v
         in zettelTitle z
    renderPandocAsText :: [Inline] -> Maybe Text
    renderPandocAsText =
      either (const Nothing) Just . runPure . writePlain def . Pandoc mempty . pure . Plain

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

renderRouteBody :: PandocBuilder t m => Config -> Route a -> (ZettelGraph, a) -> NeuronWebT t m (RouteError a)
renderRouteBody config r (g, x) = do
  case r of
    Route_ZIndex -> do
      divClass "ui text container" $ do
        renderIndex config g x
        renderBrandFooter
      pure mempty
    Route_Search {} -> do
      divClass "ui text container" $ do
        renderSearch g
        renderBrandFooter
      pure mempty
    Route_Zettel _ -> do
      errs <-
        ZettelView.renderZettel
          (editUrl config)
          (Tagged $ Just "zettel-container-anchor")
          (g, x)
      renderBrandFooter
      pure errs
    Route_Redirect _ -> do
      elAttr "meta" ("http-equiv" =: "Refresh" <> "content" =: ("0; url=" <> (Rib.routeUrlRel $ Route_Zettel x))) blank
      pure mempty

renderErrors :: DomBuilder t m => Map ZettelID (Either Text [QueryError]) -> m ()
renderErrors errors = do
  let skippedZettels = Map.mapMaybe leftToMaybe errors
      zettelsWithErrors = Map.mapMaybe rightToMaybe errors
  unless (null skippedZettels) $ do
    divClass "ui small negative message" $ do
      divClass "header" $ do
        text "These files are excluded from the zettelkasten due to parse errors"
      el "p" $ do
        el "ol" $ do
          forM_ (Map.toList skippedZettels) $ \(zid, err) ->
            el "li" $ do
              el "b" $ el "tt" $ text $ toText $ zettelIDSourceFileName zid
              text ": "
              el "pre" $ text err
  forM_ (Map.toList zettelsWithErrors) $ \(zid, qerrors) ->
    divClass "ui tiny warning message" $ do
      divClass "header" $ do
        text $ "Zettel "
        elClass "span" "zettel-link-container" $ do
          elClass "span" "zettel-link" $ do
            elAttr "a" ("href" =: QueryView.zettelUrl zid) $ text $ zettelIDText zid
        text " has errors"
      el "p" $ do
        el "ol" $ do
          forM_ qerrors $ \qe ->
            el "li" $ el "pre" $ text $ showQueryError qe

renderIndex :: DomBuilder t m => Config -> ZettelGraph -> Map ZettelID (Either Text [QueryError]) -> NeuronWebT t m ()
renderIndex Config {..} graph errors = do
  let neuronTheme = Theme.mkTheme theme
  elClass "h1" "header" $ text "Zettel Index"
  renderErrors errors
  divClass "z-index" $ do
    -- Cycle detection.
    case G.topSort graph of
      Left (toList -> cyc) -> divClass "ui orange segment" $ do
        el "h2" $ text "Cycle detected"
        forM_ cyc $ \zettel ->
          el "li" $ QueryView.renderZettelLink Nothing def zettel
      _ -> blank
    let clusters = G.categoryClusters graph
    el "p" $ do
      text $ "There " <> countNounBe "cluster" "clusters" (length clusters) <> " in the Zettelkasten folgezettel graph. "
      text "Each cluster is rendered as a forest."
    forM_ clusters $ \forest ->
      divClass ("ui " <> Theme.semanticColor neuronTheme <> " segment") $ do
        -- Forest of zettels, beginning with mother vertices.
        el "ul" $ renderForest True Nothing (Just graph) forest
  el "br" blank
  where
    countNounBe noun nounPlural = \case
      1 -> "is 1 " <> noun
      n -> "are " <> show n <> " " <> nounPlural

renderSearch :: DomBuilder t m => ZettelGraph -> m ()
renderSearch graph = do
  elClass "h1" "header" $ text "Search"
  divClass "ui fluid icon input search" $ do
    elAttr "input" ("type" =: "text" <> "id" =: "search-input") blank
    fa "search icon fas fa-search"
  divClass "ui hidden divider" blank
  let allZettels = G.getZettels graph
      allTags = Set.fromList $ concatMap zettelTags allZettels
      index = object ["zettels" .= fmap (object . zettelJson) allZettels, "tags" .= allTags]
  elAttr "div" ("class" =: "ui fluid multiple search selection dropdown" <> "id" =: "search-tags") $ do
    elAttr "input" ("name" =: "tags" <> "type" =: "hidden") blank
    elClass "i" "dropdown icon" blank
    divClass "default text" $ text "Select tags…"
    divClass "menu" $ do
      forM_ allTags $ \t -> do
        divClass "item" $ text (unTag t)
  divClass "ui divider" blank
  elAttr "ul" ("id" =: "search-results" <> "class" =: "zettel-list") blank
  el "script" $ text $ "let index = " <> toText (Aeson.encodeToLazyText index) <> ";"
  el "script" $ text searchScript

renderBrandFooter :: DomBuilder t m => m ()
renderBrandFooter =
  divClass "ui one column grid footer-version" $ do
    divClass "center aligned column" $ do
      el "p" $ do
        text "Generated by "
        elAttr "a" ("href" =: "https://neuron.zettel.page") $ text "Neuron"
        text " "
        el "code" $ text neuronVersion

-- | Font awesome element
fa :: DomBuilder t m => Text -> m ()
fa k = elClass "i" k blank

-- | Used in z-index page
renderForest ::
  DomBuilder t m =>
  Bool ->
  Maybe Int ->
  -- When given the zettelkasten graph, also show non-parent backlinks.
  -- The dfsForest tree is "incomplete" in that it lacks these references.
  Maybe ZettelGraph ->
  [Tree Zettel] ->
  NeuronWebT t m ()
renderForest isRoot maxLevel mg trees =
  case maxLevel of
    Just 0 -> blank
    _ -> do
      forM_ (sortForest trees) $ \(Node zettel subtrees) ->
        el "li" $ do
          let zettelDiv =
                divClass
                  (maybe "" (const "ui ") mg)
          bool id zettelDiv isRoot $
            QueryView.renderZettelLink Nothing def zettel
          whenJust mg $ \g -> do
            text " "
            case G.backlinks Folgezettel zettel g of
              conns@(_ : _ : _) ->
                -- Has two or more category backlinks
                forM_ conns $ \zettel2 -> do
                  let connTitle = (zettelIDText (zettelID zettel2) <> " " <> zettelTitle zettel2)
                  elAttr "i" ("class" =: "fas fa-link" <> "title" =: connTitle) blank
              _ -> blank
          when (length subtrees > 0) $ do
            el "ul" $ renderForest False ((\n -> n - 1) <$> maxLevel) mg subtrees
  where
    -- Sort trees so that trees containing the most recent zettel (by ID) come first.
    sortForest = reverse . sortOn maximum

style :: Config -> Css
style Config {..} = do
  let neuronTheme = Theme.mkTheme theme
  ".ui.label span.fas" ? do
    C.marginRight $ em 0.3
  "div.z-index" ? do
    C.ul ? do
      C.listStyleType C.square
      C.paddingLeft $ em 1.5
  ZettelCSS.zettelCss neuronTheme
  "div.tag-tree" ? do
    "div.node" ? do
      C.fontWeight C.bold
      "a.inactive" ? do
        C.color "#555"
  ".footer-version, .footer-version a, .footer-version a:visited" ? do
    C.color gray
  ".footer-version a" ? do
    C.fontWeight C.bold
  ".footer-version" ? do
    C.fontSize $ em 0.7
