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

import Clay hiding (id, ms, object, reverse, s, style, type_)
import qualified Clay as C
import Data.Aeson ((.=), object)
import qualified Data.Aeson.Text as Aeson
import Data.Default (def)
import Data.FileEmbed (embedStringFile)
import Data.Foldable (maximum)
import qualified Data.Set as Set
import Data.Structured.Breadcrumb (Breadcrumb)
import qualified Data.Structured.Breadcrumb as Breadcrumb
import Data.TagTree (Tag (..))
import Data.Time.ISO8601 (formatISO8601)
import Data.Tree (Tree (..))
import Neuron.Config
import Neuron.Version (neuronVersion)
import Neuron.Web.Route
import qualified Neuron.Web.Theme as Theme
import Neuron.Zettelkasten.Connection
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.Graph (ZettelGraph)
import Neuron.Zettelkasten.ID (ZettelID (..), zettelIDSourceFileName, zettelIDText)
import Neuron.Zettelkasten.Zettel
import qualified Neuron.Zettelkasten.Zettel.View as ZettelView
import Reflex.Dom.Core
import Reflex.Dom.Pandoc.Document (PandocBuilder)
import Relude
import qualified Rib
import Rib.Extra.OpenGraph
import qualified Skylighting.Format.HTML as Skylighting
import qualified Skylighting.Styles as Skylighting
import qualified Text.URI as URI

searchScript :: Text
searchScript = $(embedStringFile "./src-js/search.js")

renderRouteHead :: DomBuilder t m => Config -> Route graph a -> (graph, a) -> m ()
renderRouteHead config r val = do
  elAttr "meta" ("http-equiv" =: "Content-Type" <> "content" =: "text/html; charset=utf-8") blank
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
  el "title" $ text $ routeTitle config (snd val) r
  elAttr "link" ("rel" =: "shortcut icon" <> "href" =: "https://raw.githubusercontent.com/srid/neuron/master/assets/logo.ico") blank
  case r of
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
      renderOpenGraph $ routeOpenGraph config (snd val) r
      Breadcrumb.renderBreadcrumbs $ routeStructuredData config val r
      elAttr "style" ("type" =: "text/css") $ text $ toText $ Skylighting.styleToCss Skylighting.tango
  where
    routeStructuredData :: Config -> (g, a) -> Route g a -> [Breadcrumb]
    routeStructuredData Config {..} (graph, v) = \case
      Route_Zettel _ ->
        case siteBaseUrl of
          Nothing -> []
          Just baseUrl ->
            let mkCrumb :: Zettel -> Breadcrumb.Item
                mkCrumb Zettel {..} =
                  Breadcrumb.Item zettelTitle (Just $ routeUri baseUrl $ Route_Zettel zettelID)
             in Breadcrumb.fromForest $ fmap mkCrumb <$> G.backlinkForest Folgezettel v graph
      _ ->
        []

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

renderRouteBody :: PandocBuilder t m => Config -> Route graph a -> (graph, a) -> m ()
renderRouteBody config r (g, x) = do
  case r of
    Route_ZIndex ->
      renderIndex config g
    Route_Search {} ->
      renderSearch g
    Route_Zettel _ ->
      renderZettel config (g, x)
    Route_Redirect _ ->
      elAttr "meta" ("http-equiv" =: "Refresh" <> "content" =: ("0; url=" <> (Rib.routeUrlRel $ Route_Zettel x))) blank

renderIndex :: DomBuilder t m => Config -> ZettelGraph -> m ()
renderIndex Config {..} graph = do
  let neuronTheme = Theme.mkTheme theme
  elClass "h1" "header" $ text "Zettel Index"
  divClass "z-index" $ do
    -- Cycle detection.
    case G.topSort graph of
      Left (toList -> cyc) -> divClass "ui orange segment" $ do
        el "h2" $ text "Cycle detected"
        forM_ cyc $ \zettel ->
          el "li" $ ZettelView.renderZettelLink def zettel
      _ -> blank
    let clusters = G.categoryClusters graph
    el "p" $ do
      text $ "There " <> countNounBe "cluster" "clusters" (length clusters) <> " in the Zettelkasten graph. "
      text "Each cluster is rendered as a forest, with their roots (mother zettels) highlighted."
    forM_ clusters $ \forest ->
      divClass ("ui stacked " <> Theme.semanticColor neuronTheme <> " segment") $ do
        -- Forest of zettels, beginning with mother vertices.
        el "ul" $ renderForest True Nothing (Just graph) forest
    renderBrandFooter True
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

renderZettel :: PandocBuilder t m => Config -> (ZettelGraph, Zettel) -> m ()
renderZettel config (graph, z@Zettel {..}) = do
  divClass "zettel-view" $ do
    ZettelView.renderZettelContent z
    renderZettelPanel config graph z

renderZettelPanel :: DomBuilder t m => Config -> ZettelGraph -> Zettel -> m ()
renderZettelPanel Config {..} graph z@Zettel {..} = do
  let neuronTheme = Theme.mkTheme theme
  divClass ("ui inverted " <> Theme.semanticColor neuronTheme <> " top attached connections segment") $ do
    divClass "ui two column grid" $ do
      divClass "column" $ do
        divClass "ui header" $ text "Down"
        el "ul" $ renderForest True (Just 2) Nothing $
          G.frontlinkForest Folgezettel z graph
      divClass "column" $ do
        divClass "ui header" $ text "Up"
        el "ul" $ do
          renderForest True Nothing Nothing $
            G.backlinkForest Folgezettel z graph
        divClass "ui header" $ text "Other backlinks"
        el "ul" $ do
          renderForest True Nothing Nothing
            $ fmap (flip Node [])
            $ G.backlinks OrdinaryConnection z graph
  divClass "ui inverted black bottom attached footer segment" $ do
    divClass "ui equal width grid" $ do
      divClass "center aligned column" $ do
        let homeUrl = maybe "." (const "index.html") $ G.getZettel (ZettelCustomID "index") graph
        elAttr "a" ("href" =: homeUrl <> "title" =: "/") $ fa "fas fa-home"
      whenJust editUrl $ \urlPrefix ->
        divClass "center aligned column" $ do
          elAttr "a" ("href" =: (urlPrefix <> toText (zettelIDSourceFileName zettelID)) <> "title" =: "Edit this Zettel") $ fa "fas fa-edit"
      divClass "center aligned column" $ do
        elAttr "a" ("href" =: (Rib.routeUrlRel Route_Search) <> "title" =: "Search Zettels") $ fa "fas fa-search"
      divClass "center aligned column" $ do
        elAttr "a" ("href" =: (Rib.routeUrlRel Route_ZIndex) <> "title" =: "All Zettels (z-index)") $
          fa "fas fa-tree"
  renderBrandFooter False

renderBrandFooter :: DomBuilder t m => Bool -> m ()
renderBrandFooter withVersion =
  divClass "ui one column grid footer-version" $ do
    divClass "center aligned column" $ do
      el "p" $ do
        text "Generated by "
        elAttr "a" ("href" =: "https://neuron.zettel.page") $ text "Neuron"
        when withVersion $ do
          text " "
          el "code" $ text neuronVersion

-- | Font awesome element
fa :: DomBuilder t m => Text -> m ()
fa k = elClass "i" k blank

renderForest ::
  DomBuilder t m =>
  Bool ->
  Maybe Int ->
  -- When given the zettelkasten graph, also show non-parent backlinks.
  -- The dfsForest tree is "incomplete" in that it lacks these references.
  Maybe ZettelGraph ->
  [Tree Zettel] ->
  m ()
renderForest isRoot maxLevel mg trees =
  case maxLevel of
    Just 0 -> blank
    _ -> do
      forM_ (sortForest trees) $ \(Node zettel subtrees) ->
        el "li" $ do
          let zettelDiv =
                divClass
                  (maybe "" (const "ui black label") mg)
          bool id zettelDiv isRoot $
            ZettelView.renderZettelLink def zettel
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
      linkColor = Theme.withRgb neuronTheme C.rgb
  ".ui.label span.fas" ? do
    C.marginRight $ em 0.3
  "span.zettel-link-container span.zettel-link a" ? do
    C.fontWeight C.bold
    C.color linkColor
    C.textDecoration C.none
  "span.zettel-link-container span.zettel-link a:hover" ? do
    C.backgroundColor linkColor
    C.color C.white
  "span.zettel-link-container span.extra" ? do
    C.color C.auto
    C.paddingRight $ em 0.3
  "div.z-index" ? do
    C.ul ? do
      C.listStyleType C.square
      C.paddingLeft $ em 1.5
  "div.zettel-view" ? do
    -- This list styling applies both to zettel content, and the rest of the
    -- view (eg: connections pane)
    C.ul ? do
      C.paddingLeft $ em 1.5
      C.listStyleType C.square
      C.li ? do
        mempty -- C.paddingBottom $ em 1
    ZettelView.zettelCss neuronTheme
  "div.tag-tree" ? do
    "div.node" ? do
      C.fontWeight C.bold
      "a.inactive" ? do
        C.color "#555"
  "div.connections" ? do
    "a" ? do
      C.important $ color white
    "a:hover" ? do
      C.opacity 0.5
  ".footer" ? do
    "a" ? do
      C.color white
  ".footer-version, .footer-version a, .footer-version a:visited" ? do
    C.color gray
  ".footer-version a" ? do
    C.fontWeight C.bold
  ".footer-version" ? do
    C.fontSize $ em 0.7
  "[data-tooltip]:after" ? do
    C.fontSize $ em 0.7
