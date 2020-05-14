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
import Neuron.Config
import Neuron.Version (neuronVersion)
import Neuron.Web.Route
import qualified Neuron.Web.Theme as Theme
import Neuron.Zettelkasten.Connection
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.Graph (ZettelGraph)
import Neuron.Zettelkasten.ID (ZettelID (..), zettelIDSourceFileName, zettelIDText)
import Neuron.Zettelkasten.Query.Theme (LinkView (..))
import Neuron.Zettelkasten.Query.View (zettelUrl)
import Neuron.Zettelkasten.Zettel
import Reflex.Dom.Core
import Reflex.Dom.Pandoc.Document (elPandocDoc)
import Relude
import qualified Rib
import Rib.Extra.CSS (mozillaKbdStyle)
import qualified Skylighting.Format.HTML as Skylighting
import qualified Skylighting.Styles as Skylighting
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
      style_ [type_ "text/css"] $ Skylighting.styleToCss Skylighting.tango
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

renderRouteBody :: DomBuilder t m => Config -> Route graph a -> (graph, a) -> m ()
renderRouteBody config r (g, x) = do
  case r of
    Route_ZIndex ->
      renderIndex config g
    Route_Search {} ->
      renderSearch g
    Route_Zettel zid ->
      renderZettel config (g, x) zid
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
          el "li" $ renderZettelLink def zettel
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

renderZettelContent :: DomBuilder t m => Zettel -> m ()
renderZettelContent Zettel {..} = do
  divClass "ui raised segment zettel-content" $ do
    elClass "h1" "header" $ text zettelTitle
    elPandocDoc zettelContent
    renderTags zettelTags
    whenJust zettelDay $ \day ->
      elAttr "div" ("class" =: "date" <> "title" =: "Zettel creation date") $ text $ show day

renderZettel :: DomBuilder t m => Config -> (ZettelGraph, Zettel) -> ZettelID -> m ()
renderZettel Config {..} (graph, z@Zettel {..}) zid = do
  let neuronTheme = Theme.mkTheme theme
  divClass "zettel-view" $ do
    renderZettelContent z
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
            elAttr "a" ("href" =: (urlPrefix <> toText (zettelIDSourceFileName zid)) <> "title" =: "Edit this Zettel") $ fa "fas fa-edit"
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

renderTags :: DomBuilder t m => [Tag] -> m ()
renderTags tags = do
  forM_ tags $ \(unTag -> t) -> do
    -- NOTE(ui): Ideally this should be at the top, not bottom. But putting it at
    -- the top pushes the zettel content down, introducing unnecessary white
    -- space below the title. So we put it at the bottom for now.
    elAttr "span" ("class" =: "ui black right ribbon label" <> "title" =: "Tag") $ do
      elAttr
        "a"
        ( "href" =: (routeUrlRelWithQuery Route_Search [queryKey|t|] t)
            <> "title" =: ("See all zettels tagged '" <> t <> "'")
        )
        $ text t
    el "p" blank

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
            renderZettelLink def zettel
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

-- | Render a link to an individual zettel.
renderZettelLink :: forall t m. DomBuilder t m => Maybe LinkView -> Zettel -> m ()
renderZettelLink (fromMaybe def -> LinkView {..}) Zettel {..} = do
  let mextra =
        if linkViewShowDate
          then case zettelDay of
            Just day ->
              Just $ show @Text day
            Nothing ->
              Nothing
          else Nothing
  elClass "span" "zettel-link-container" $ do
    forM_ mextra $ \extra ->
      elClass "span" "extra monoFont" $ text extra
    let linkTooltip =
          if null zettelTags
            then Nothing
            else Just $ "Tags: " <> T.intercalate "; " (unTag <$> zettelTags)
    elAttr "span" ("class" =: "zettel-link" <> withTooltip linkTooltip) $ do
      elAttr "a" ("href" =: (zettelUrl zettelID)) $ text zettelTitle
  where
    withTooltip :: Maybe Text -> Map Text Text
    withTooltip = \case
      Nothing -> mempty
      Just s ->
        ( "data-tooltip" =: s
            <> "data-inverted" =: ""
            <> "data-position" =: "right center"
        )

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
    "div.zettel-content" ? do
      -- All of these apply to the zettel content card only.
      "div.date" ? do
        C.textAlign C.center
        C.color C.gray
      C.h1 ? do
        C.paddingTop $ em 0.2
        C.paddingBottom $ em 0.2
        C.textAlign C.center
        C.backgroundColor $ Theme.withRgb neuronTheme C.rgba 0.1
      C.h2 ? do
        C.borderBottom C.solid (px 1) C.steelblue
        C.marginBottom $ em 0.5
      C.h3 ? do
        C.margin (px 0) (px 0) (em 0.4) (px 0)
      C.h4 ? do
        C.opacity 0.8
      "div#footnotes" ? do
        C.marginTop $ em 4
        C.borderTop C.groove (px 2) linkColor
        C.fontSize $ em 0.9
      -- reflex-dom-pandoc footnote aside elements
      -- (only used for footnotes defined inside footnotes)
      "aside.footnote-inline" ? do
        C.width $ pct 30
        C.paddingLeft $ px 15
        C.marginLeft $ px 15
        C.float C.floatRight
        C.backgroundColor C.lightgray
      -- CSS library for users to use in their Pandoc attributes blocks
      ".overflows" ? do
        C.overflow auto
      codeStyle
      blockquoteStyle
      kbd ? mozillaKbdStyle
  -- End of div.zettel-content
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
      "div.pandoc-code" ? do
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
