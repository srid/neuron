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
import Reflex.Dom.Core hiding ((&))
import Reflex.Dom.Pandoc.Document (PandocBuilder)
import Relude hiding ((&))
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
      renderSearch config g
    Route_Zettel _ ->
      renderZettel config (g, x)
    Route_Redirect _ ->
      elAttr "meta" ("http-equiv" =: "Refresh" <> "content" =: ("0; url=" <> (Rib.routeUrlRel $ Route_Zettel x))) blank

renderIndex :: DomBuilder t m => Config -> ZettelGraph -> m ()
renderIndex config@Config {..} graph = divClass "ui text container" $ do
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
      divClass ("ui " <> Theme.semanticColor neuronTheme <> " segment") $ do
        -- Forest of zettels, beginning with mother vertices.
        el "ul" $ renderForest True Nothing (Just graph) forest
    renderFooter config graph Nothing
    renderBrandFooter
  where
    countNounBe noun nounPlural = \case
      1 -> "is 1 " <> noun
      n -> "are " <> show n <> " " <> nounPlural

renderSearch :: DomBuilder t m => Config -> ZettelGraph -> m ()
renderSearch config graph = divClass "ui text container" $ do
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
  renderFooter config graph Nothing
  renderBrandFooter

renderZettel :: PandocBuilder t m => Config -> (ZettelGraph, Zettel) -> m ()
renderZettel config (graph, z@Zettel {..}) = do
  let upTree = G.backlinkForest Folgezettel z graph
  whenNotNull upTree $ \_ -> do
    elAttr "div" ("class" =: "flipped tree" <> "style" =: "transform-origin: 50%") $ do
      el "ul" $ do
        el "li" $ do
          divClass "forest-link" $ el "a" $ text zettelTitle
          el "ul" $ do
            renderForestNG True Nothing Nothing upTree
  divClass "ui text container" $ do
    divClass "zettel-view" $ do
      ZettelView.renderZettelContent z
      let cfBacklinks = G.backlinks OrdinaryConnection z graph
      whenNotNull cfBacklinks $ \_ -> divClass "ui attached segment backlinks" $ do
        elAttr "div" ("class" =: "ui header" <> title =: "Zettels that link here, but without branching") $
          text "More backlinks"
        el "ul" $ do
          renderForest True Nothing Nothing $
            fmap (flip Node []) cfBacklinks
      renderFooter config graph (Just z)
  -- renderZettelPanel config graph z
  -- elAttr "div" ("class" =: "tree" <> "style" =: "transform-origin: 50%") $ do
  --   el "ul" $ do
  --     el "li" $ do
  --       divClass "forest-link" $ el "a" $ text zettelTitle
  --       el "ul" $ do
  --         renderForestNG True (Just 2) Nothing $ G.frontlinkForest Folgezettel z graph
  renderBrandFooter

_renderZettelPanel :: DomBuilder t m => Config -> ZettelGraph -> Zettel -> m ()
_renderZettelPanel config@Config {..} graph z@Zettel {..} = do
  let neuronTheme = Theme.mkTheme theme
  divClass ("ui inverted " <> Theme.semanticColor neuronTheme <> " top attached connections segment") $ do
    divClass "ui two column grid" $ do
      divClass "column" $ do
        elAttr "div" ("class" =: "ui header" <> title =: "The following zettels branch to this zettel") $
          text "Uplinks"
        el "ul" $ do
          renderForest True Nothing Nothing $
            G.backlinkForest Folgezettel z graph
        let cfBacklinks = G.backlinks OrdinaryConnection z graph
        whenNotNull cfBacklinks $ \_ -> do
          elAttr "div" ("class" =: "ui header" <> title =: "Zettels that link here, but without branching") $
            text "Backlinks"
          el "ul" $ do
            renderForest True Nothing Nothing $
              fmap (flip Node []) cfBacklinks
      divClass "column" $ do
        elAttr "div" ("class" =: "ui header" <> title =: "This zettel branches to the following zettels") $
          text "Downlinks"
        el "ul" $ renderForest True (Just 2) Nothing $
          G.frontlinkForest Folgezettel z graph
  renderFooter config graph (Just z)
  renderBrandFooter

renderFooter :: DomBuilder t m => Config -> ZettelGraph -> Maybe Zettel -> m ()
renderFooter Config {..} graph mzettel = do
  let attachClass = maybe "" (const "bottom attached") mzettel
  divClass ("ui inverted black " <> attachClass <> " footer segment") $ do
    divClass "ui equal width grid" $ do
      divClass "center aligned column" $ do
        let homeUrl = maybe "." (const "index.html") $ G.getZettel (ZettelCustomID "index") graph
        elAttr "a" ("href" =: homeUrl <> "title" =: "/") $ fa "fas fa-home"
      whenJust ((,) <$> mzettel <*> editUrl) $ \(Zettel {..}, urlPrefix) ->
        divClass "center aligned column" $ do
          elAttr "a" ("href" =: (urlPrefix <> toText (zettelIDSourceFileName zettelID)) <> "title" =: "Edit this Zettel") $ fa "fas fa-edit"
      divClass "center aligned column" $ do
        elAttr "a" ("href" =: (Rib.routeUrlRel Route_Search) <> "title" =: "Search Zettels") $ fa "fas fa-search"
      divClass "center aligned column" $ do
        elAttr "a" ("href" =: (Rib.routeUrlRel Route_ZIndex) <> "title" =: "All Zettels (z-index)") $
          fa "fas fa-tree"

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

renderForestNG ::
  DomBuilder t m =>
  Bool ->
  Maybe Int ->
  -- When given the zettelkasten graph, also show non-parent backlinks.
  -- The dfsForest tree is "incomplete" in that it lacks these references.
  Maybe ZettelGraph ->
  [Tree Zettel] ->
  m ()
renderForestNG _isRoot maxLevel mg trees =
  case maxLevel of
    Just 0 -> blank
    _ -> do
      forM_ (sortForest trees) $ \(Node zettel subtrees) ->
        el "li" $ do
          let linkDivClass = maybe "forest-link" (const "ui black label forest-link") mg
          divClass linkDivClass $
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
            el "ul" $ renderForestNG False ((\n -> n - 1) <$> maxLevel) mg subtrees
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
  pureCssTreeDiagram
  ".backlinks" ? do
    opacity 0.5

-- https://codepen.io/philippkuehn/pen/QbrOaN
pureCssTreeDiagram :: Css
pureCssTreeDiagram = do
  -- TODO: should only apply for folgezettel
  ".zettel-link-container::after" ? do
    C.paddingLeft $ em 0.3
    C.content $ stringContent "ᛦ"
  let cellBorderWidth = px 2
      flipTree = False
      rotateDeg = deg 180
  ".tree.flipped" ? do
    C.transform $ C.rotate rotateDeg
  ".tree" ? do
    C.overflow auto
    fontSize $ em 0.9
    when flipTree $ do
      C.transform $ C.rotate rotateDeg
    -- Clay does not support this; doing it inline in div style.
    -- C.transformOrigin $ pct 50
    "ul" ? do
      C.position relative
      sym2 C.padding (em 1) 0
      C.whiteSpace nowrap
      sym2 C.margin (px 0) auto
      C.textAlign center
      C.after & do
        C.content $ stringContent ""
        C.display C.displayTable
        C.clear both
    "li" ? do
      C.display C.inlineBlock
      C.verticalAlign C.vAlignTop
      C.textAlign C.center
      C.listStyleType none
      C.position relative
      C.padding (em 1) (em 0.5) (em 0) (em 0.5)
      forM_ [C.before, C.after] $ \sel -> sel & do
        C.content $ stringContent ""
        C.position absolute
        C.top $ px 0
        C.right $ pct 50
        C.borderTop solid cellBorderWidth "#ccc"
        C.width $ pct 50
        C.height $ em 1.2
      C.after & do
        C.right auto
        C.left $ pct 50
        C.borderLeft solid cellBorderWidth "#ccc"
      C.onlyChild & do
        C.paddingTop $ em 0
        forM_ [C.after, C.before] $ \sel -> sel & do
          C.display none
      C.firstChild & do
        C.before & do
          C.borderStyle none
          C.borderWidth $ px 0
        C.after & do
          C.borderRadius (px 5) 0 0 0
      C.lastChild & do
        C.after & do
          C.borderStyle none
          C.borderWidth $ px 0
        C.before & do
          C.borderRight solid cellBorderWidth "#ccc"
          C.borderRadius 0 (px 5) 0 0
    "ul ul::before" ? do
      C.content $ stringContent ""
      C.position absolute
      C.top $ px 0
      C.left $ pct 50
      C.borderLeft solid cellBorderWidth "#ccc"
      C.width $ px 0
      C.height $ em 1.2
    "li" ? do
      "div.forest-link" ? do
        border solid cellBorderWidth "#ccc"
        sym2 C.padding (em 0.5) (em 0.75)
        C.textDecoration none
        C.display inlineBlock
        sym C.borderRadius (px 5)
        C.color "#333"
        C.position relative
        C.top cellBorderWidth
        when flipTree $ do
          C.transform $ C.rotate rotateDeg
  ".tree.flipped li div.forest-link" ? do
    C.transform $ C.rotate rotateDeg
