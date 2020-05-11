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
import qualified Data.Text as T
import Data.Tree (Tree (..))
import Lucid
import Lucid.Base (makeAttribute)
import Neuron.Config
import Neuron.Version (neuronVersionFull)
import Neuron.Web.Route
import qualified Neuron.Web.Theme as Theme
import Neuron.Zettelkasten.Connection
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.Graph (ZettelGraph)
import Neuron.Zettelkasten.ID (ZettelID (..), zettelIDSourceFileName, zettelIDText)
import Neuron.Zettelkasten.Query.Theme (LinkView (..))
import Neuron.Zettelkasten.Query.View (zettelUrl)
import Neuron.Zettelkasten.Zettel
import Relude
import qualified Rib
import Rib.Extra.CSS (mozillaKbdStyle)
import qualified Rib.Parser.Pandoc as Pandoc
import Text.Pandoc.Highlighting (styleToCss, tango)
import Text.URI.QQ

searchScript :: Text
searchScript = $(embedStringFile "./src-js/search.js")

renderRouteHead :: Monad m => Config -> Route graph a -> (graph, a) -> HtmlT m ()
renderRouteHead config r val = do
  meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
  meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
  title_ $ toHtml $ routeTitle config (snd val) r
  link_ [rel_ "shortcut icon", href_ "https://raw.githubusercontent.com/srid/neuron/master/assets/logo.ico"]
  case r of
    Route_Redirect _ ->
      mempty
    Route_Search {} -> do
      with (script_ mempty) [src_ "https://cdn.jsdelivr.net/npm/jquery@3.5.0/dist/jquery.min.js"]
      with (script_ mempty) [src_ "https://cdn.jsdelivr.net/npm/semantic-ui@2.4.2/dist/semantic.min.js"]
      with (script_ mempty) [src_ "https://cdn.jsdelivr.net/npm/js-search@2.0.0/dist/umd/js-search.min.js"]
    _ -> do
      toHtml $ routeOpenGraph config (snd val) r
      toHtml $ routeStructuredData config val r
      style_ [type_ "text/css"] $ styleToCss tango
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

renderRouteBody :: Monad m => Config -> Route graph a -> (graph, a) -> HtmlT m ()
renderRouteBody config r (g, x) = do
  case r of
    Route_ZIndex ->
      renderIndex config g
    Route_Search {} ->
      renderSearch g
    Route_Zettel zid ->
      renderZettel config (g, x) zid
    Route_Redirect _ ->
      meta_ [httpEquiv_ "Refresh", content_ $ "0; url=" <> (Rib.routeUrlRel $ Route_Zettel x)]

renderIndex :: Monad m => Config -> ZettelGraph -> HtmlT m ()
renderIndex Config {..} graph = do
  let neuronTheme = Theme.mkTheme theme
  h1_ [class_ "header"] $ "Zettel Index"
  div_ [class_ "z-index"] $ do
    -- Cycle detection.
    case G.topSort graph of
      Left (toList -> cyc) -> div_ [class_ "ui orange segment"] $ do
        h2_ "Cycle detected"
        forM_ cyc $ \zettel ->
          li_ $ renderZettelLink def zettel
      _ -> mempty
    let clusters = G.categoryClusters graph
    p_ $ do
      "There " <> countNounBe "cluster" "clusters" (length clusters) <> " in the Zettelkasten graph. "
      "Each cluster is rendered as a forest, with their roots (mother zettels) highlighted."
    forM_ clusters $ \forest ->
      div_ [class_ $ "ui stacked " <> Theme.semanticColor neuronTheme <> " segment"] $ do
        -- Forest of zettels, beginning with mother vertices.
        ul_ $ renderForest True Nothing True graph forest
    renderBrandFooter True
  where
    countNounBe noun nounPlural = \case
      1 -> "is 1 " <> noun
      n -> "are " <> show n <> " " <> nounPlural

renderSearch :: forall m. Monad m => ZettelGraph -> HtmlT m ()
renderSearch graph = do
  h1_ [class_ "header"] $ "Search"
  div_ [class_ "ui fluid icon input search"] $ do
    input_ [type_ "text", id_ "search-input"]
    fa "search icon fas fa-search"
  div_ [class_ "ui hidden divider"] mempty
  let allZettels = G.getZettels graph
      allTags = Set.fromList $ concatMap zettelTags allZettels
      index = object ["zettels" .= fmap (object . zettelJson) allZettels, "tags" .= allTags]
  div_ [class_ "ui fluid multiple search selection dropdown", id_ "search-tags"] $ do
    with (input_ mempty) [name_ "tags", type_ "hidden"]
    with (i_ mempty) [class_ "dropdown icon"]
    div_ [class_ "default text"] "Select tags…"
    div_ [class_ "menu"] $ do
      forM_ allTags $ \tag -> do
        div_ [class_ "item"] $ toHtml (unTag tag)
  div_ [class_ "ui divider"] mempty
  ul_ [id_ "search-results", class_ "zettel-list"] mempty
  script_ $ "let index = " <> toText (Aeson.encodeToLazyText index) <> ";"
  script_ searchScript

renderZettel :: forall m. Monad m => Config -> (ZettelGraph, Zettel) -> ZettelID -> HtmlT m ()
renderZettel Config {..} (graph, z@Zettel {..}) zid = do
  let neuronTheme = Theme.mkTheme theme
  div_ [class_ "zettel-view"] $ do
    div_ [class_ "ui raised segments"] $ do
      div_ [class_ "ui top attached segment"] $ do
        h1_ [class_ "header"] $ toHtml zettelTitle
        -- TODO: Use reflex-dom-pandoc eventually
        Pandoc.render zettelContent
        whenNotNull zettelTags $ \_ ->
          renderTags zettelTags
        forM_ zettelDay $ \day ->
          div_ [class_ "date", title_ "Zettel creation date"] $ toHtml $ show @Text day
    div_ [class_ $ "ui inverted " <> Theme.semanticColor neuronTheme <> " top attached connections segment"] $ do
      div_ [class_ "ui two column grid"] $ do
        div_ [class_ "column"] $ do
          div_ [class_ "ui header"] "Down"
          ul_ $ renderForest True (Just 2) False graph $
            G.frontlinkForest Folgezettel z graph
        div_ [class_ "column"] $ do
          div_ [class_ "ui header"] "Up"
          ul_ $ do
            renderForest True Nothing False graph $
              G.backlinkForest Folgezettel z graph
          div_ [class_ "ui header"] "Other backlinks"
          ul_ $ do
            renderForest True Nothing False graph
              $ fmap (flip Node [])
              $ G.backlinks OrdinaryConnection z graph
    div_ [class_ "ui inverted black bottom attached footer segment"] $ do
      div_ [class_ "ui equal width grid"] $ do
        div_ [class_ "center aligned column"] $ do
          let homeUrl = maybe "." (const "index.html") $ G.getZettel (ZettelCustomID "index") graph
          a_ [href_ homeUrl, title_ "/"] $ fa "fas fa-home"
        whenJust editUrl $ \urlPrefix ->
          div_ [class_ "center aligned column"] $ do
            a_ [href_ $ urlPrefix <> toText (zettelIDSourceFileName zid), title_ "Edit this Zettel"] $ fa "fas fa-edit"
        div_ [class_ "center aligned column"] $ do
          a_ [href_ (Rib.routeUrlRel Route_Search), title_ "Search Zettels"] $ fa "fas fa-search"
        div_ [class_ "center aligned column"] $ do
          a_ [href_ (Rib.routeUrlRel Route_ZIndex), title_ "All Zettels (z-index)"] $
            fa "fas fa-tree"
    renderBrandFooter False

renderBrandFooter :: Monad m => Bool -> HtmlT m ()
renderBrandFooter withVersion =
  div_ [class_ "ui one column grid footer-version"] $ do
    div_ [class_ "center aligned column"] $ do
      p_ $ do
        "Generated by "
        a_ [href_ "https://neuron.zettel.page"] "Neuron"
        when withVersion $ do
          " "
          code_ $ toHtml @Text neuronVersionFull

renderTags :: Monad m => [Tag] -> HtmlT m ()
renderTags tags = do
  forM_ tags $ \(unTag -> tag) -> do
    -- NOTE(ui): Ideally this should be at the top, not bottom. But putting it at
    -- the top pushes the zettel content down, introducing unnecessary white
    -- space below the title. So we put it at the bottom for now.
    span_ [class_ "ui black right ribbon label", title_ "Tag"] $ do
      a_
        [ href_ $ routeUrlRelWithQuery Route_Search [queryKey|tag|] tag,
          title_ ("See all zettels tagged '" <> tag <> "'")
        ]
        $ toHtml tag
    p_ mempty

-- | Font awesome element
fa :: Monad m => Text -> HtmlT m ()
fa k = with i_ [class_ k] mempty

renderForest ::
  Monad m =>
  Bool ->
  Maybe Int ->
  Bool ->
  ZettelGraph ->
  [Tree Zettel] ->
  HtmlT m ()
renderForest isRoot maxLevel renderingFullTree g trees =
  case maxLevel of
    Just 0 -> mempty
    _ -> do
      forM_ (sortForest trees) $ \(Node zettel subtrees) ->
        li_ $ do
          let zettelDiv =
                div_
                  [class_ $ bool "" "ui black label" renderingFullTree]
          bool id zettelDiv isRoot $
            renderZettelLink def zettel
          when renderingFullTree $ do
            " "
            case G.backlinks Folgezettel zettel g of
              conns@(_ : _ : _) ->
                -- Has two or more category backlinks
                forM_ conns $ \zettel2 -> do
                  i_ [class_ "fas fa-link", title_ $ zettelIDText (zettelID zettel2) <> " " <> zettelTitle zettel2] mempty
              _ -> mempty
          when (length subtrees > 0) $ do
            ul_ $ renderForest False ((\n -> n - 1) <$> maxLevel) renderingFullTree g subtrees
  where
    -- Sort trees so that trees containing the most recent zettel (by ID) come first.
    sortForest = reverse . sortOn maximum

-- | Render a link to an individual zettel.
-- TODO: Remove and consolidate this with pandoc AST function in `Query.View` module
renderZettelLink :: forall m. Monad m => Maybe LinkView -> Zettel -> HtmlT m ()
renderZettelLink (fromMaybe def -> LinkView {..}) Zettel {..} = do
  let mextra =
        if linkViewShowDate
          then case zettelDay of
            Just day ->
              Just $ toHtml $ show @Text day
            Nothing ->
              Nothing
          else Nothing
  span_ [class_ "zettel-link-container"] $ do
    forM_ mextra $ \extra ->
      span_ [class_ "extra monoFont"] extra
    let linkTooltip =
          if null zettelTags
            then Nothing
            else Just $ "Tags: " <> T.intercalate "; " (unTag <$> zettelTags)
    span_ ([class_ "zettel-link"] <> withTooltip linkTooltip) $ do
      a_ [href_ (zettelUrl zettelID)] $ toHtml zettelTitle
  where
    withTooltip :: Maybe Text -> [Attribute]
    withTooltip = \case
      Nothing -> []
      Just s ->
        [ makeAttribute "data-tooltip" s,
          makeAttribute "data-inverted" "",
          makeAttribute "data-position" "right center"
        ]

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
    "div.date" ? do
      C.textAlign C.center
      C.color C.gray
    C.ul ? do
      C.paddingLeft $ em 1.5
      C.listStyleType C.square
      C.li ? do
        mempty -- C.paddingBottom $ em 1
    C.h1 ? do
      C.paddingTop $ em 0.2
      C.paddingBottom $ em 0.2
      C.textAlign C.center
      C.fontWeight $ weight 700
      C.backgroundColor $ Theme.withRgb neuronTheme C.rgba 0.1
    C.h2 ? do
      C.fontWeight $ weight 600
      C.borderBottom C.solid (px 1) C.steelblue
      C.marginBottom $ em 0.5
    C.h3 ? do
      C.fontWeight $ weight 400
      C.margin (px 0) (px 0) (em 0.4) (px 0)
    C.h4 ? do
      C.fontWeight $ weight 300
      C.opacity 0.8
    codeStyle
    blockquoteStyle
    kbd ? mozillaKbdStyle
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
  where
    codeStyle = do
      C.code ? do
        sym margin auto
        fontSize $ pct 100
      -- This pretty much selects inline code elements
      "p code, li code, ol code" ? do
        sym padding $ em 0.2
        backgroundColor "#f8f8f8"
      -- This selects block code elements
      pre ? do
        sym padding $ em 0.5
        C.overflow auto
        C.maxWidth $ pct 100
      "div.source-code" ? do
        marginLeft auto
        marginRight auto
        pre ? do
          backgroundColor "#f8f8f8"
    -- https://css-tricks.com/snippets/css/simple-and-nice-blockquote-styling/
    blockquoteStyle =
      C.blockquote ? do
        C.backgroundColor "#f9f9f9"
        C.borderLeft C.solid (px 10) "#ccc"
        sym2 C.margin (em 1.5) (px 0)
        sym2 C.padding (em 0.5) (px 10)
