{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | HTML & CSS
module Neuron.Web.View
  ( renderRoutePage,
  )
where

import Clay (Css, em, gray, important, pct, px, (?))
import qualified Clay as C
import Data.Aeson (object, toJSON, (.=))
import qualified Data.Aeson.Text as Aeson
import qualified Data.Set as Set
import Data.Some (Some (Some))
import Data.TagTree (Tag (..))
import Neuron.Config.Type (Config (..))
import Neuron.Web.Common (neuronCommonStyle, neuronFonts)
import Neuron.Web.HeadHtml (HeadHtml, renderHeadHtml)
import Neuron.Web.Manifest (Manifest, renderManifest)
import qualified Neuron.Web.Query.View as QueryView
import Neuron.Web.Route
  ( NeuronWebT,
    Route (..),
    neuronRouteLink,
    routeTitle',
  )
import Neuron.Web.StructuredData (renderStructuredData)
import Neuron.Web.Theme (Theme)
import qualified Neuron.Web.Theme as Theme
import Neuron.Web.Widget (elLinkGoogleFonts, semanticIcon)
import qualified Neuron.Web.ZIndex as ZIndex
import qualified Neuron.Web.Zettel.CSS as ZettelCSS
import qualified Neuron.Web.Zettel.View as ZettelView
import Neuron.Zettelkasten.Graph (ZettelGraph)
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.ID (indexZid)
import Neuron.Zettelkasten.Zettel
  ( Zettel,
    ZettelT (..),
    sansContent,
  )
import Reflex.Dom.Core
import Reflex.Dom.Pandoc (PandocBuilder)
import Relude hiding ((&))
import qualified Skylighting.Format.HTML as Skylighting
import qualified Skylighting.Styles as Skylighting

-- | Render the given route
renderRoutePage :: PandocBuilder t m => Text -> Config -> HeadHtml -> Manifest -> Route a -> (ZettelGraph, a) -> NeuronWebT t m ()
renderRoutePage neuronVersion config headHtml manifest r val = do
  -- DOCTYPE declaration is helpful for code that might appear in the user's `head.html` file (e.g. KaTeX).
  el "!DOCTYPE html" blank
  elAttr "html" ("lang" =: "en") $ do
    el "head" $ do
      renderRouteHead config headHtml manifest r (snd val) $ do
        renderStructuredData config r val
    el "body" $ do
      renderRouteBody neuronVersion config r val

renderRouteHead ::
  PandocBuilder t m =>
  Config ->
  HeadHtml ->
  Manifest ->
  Route a ->
  a ->
  -- | Extra widget to put in Head
  NeuronWebT t m () ->
  NeuronWebT t m ()
renderRouteHead config headHtml manifest route val extra = do
  elAttr "meta" ("http-equiv" =: "Content-Type" <> "content" =: "text/html; charset=utf-8") blank
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
  el "title" $ text $ routeTitle config val route
  renderManifest manifest
  renderCommon
  extra
  case route of
    Route_Search {} -> do
      forM_
        [ "https://cdn.jsdelivr.net/npm/jquery@3.5.0/dist/jquery.min.js",
          "https://cdn.jsdelivr.net/npm/fomantic-ui@2.8.7/dist/semantic.min.js",
          "https://cdn.jsdelivr.net/npm/js-search@2.0.0/dist/umd/js-search.min.js"
        ]
        $ \scrpt -> do
          elAttr "script" ("src" =: scrpt) blank
    _ -> do
      elAttr "style" ("type" =: "text/css") $ text $ toText $ Skylighting.styleToCss Skylighting.tango
  where
    renderCommon = do
      let neuronCss = toText $ C.renderWith C.compact [] style
      elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://cdn.jsdelivr.net/npm/fomantic-ui@2.8.7/dist/semantic.min.css") blank
      elAttr "style" ("type" =: "text/css") $ text neuronCss
      elLinkGoogleFonts neuronFonts
      renderHeadHtml headHtml
    routeTitle :: Config -> a -> Route a -> Text
    routeTitle Config {..} v =
      withSuffix siteTitle . routeTitle' v
      where
        withSuffix suffix x =
          if x == suffix
            then x
            else x <> " - " <> suffix

renderRouteBody :: PandocBuilder t m => Text -> Config -> Route a -> (ZettelGraph, a) -> NeuronWebT t m ()
renderRouteBody neuronVersion Config {..} r (g, x) = do
  let neuronTheme = Theme.mkTheme theme
      themeSelector = toText $ Theme.themeIdentifier neuronTheme
      indexZettel = G.getZettel indexZid g
  elAttr "div" ("class" =: "ui fluid container" <> "id" =: themeSelector) $ do
    case r of
      Route_ZIndex -> do
        actionsNav neuronTheme indexZettel Nothing
        divClass "ui text container" $ do
          let zIndexData = ZIndex.buildZIndex g x
          ZIndex.renderZIndex neuronTheme zIndexData
          renderBrandFooter $ Just neuronVersion
        pure mempty
      Route_Search {} -> do
        actionsNav neuronTheme indexZettel Nothing
        divClass "ui text container" $ do
          renderSearch g x
          renderBrandFooter $ Just neuronVersion
        pure mempty
      Route_Zettel _ -> do
        -- Don't inject neuron verison in zettel pages, to prevent unnecessary rebuilds when upgrading neuron
        let noVersion = Nothing
            zettelEditUrl = (<> toText (zettelPath $ sansContent x)) <$> editUrl
        actionsNav neuronTheme indexZettel zettelEditUrl
        ZettelView.renderZettel (g, x)
          <* renderBrandFooter noVersion

renderSearch :: DomBuilder t m => ZettelGraph -> Text -> m ()
renderSearch graph script = do
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
  el "script" $ text script
  where
    zettelJson Zettel {..} =
      [ "id" .= toJSON zettelID,
        "slug" .= toJSON zettelSlug,
        "title" .= zettelTitle,
        "tags" .= zettelTags,
        "day" .= zettelDate
      ]

renderBrandFooter :: DomBuilder t m => Maybe Text -> m ()
renderBrandFooter mver =
  divClass "ui center aligned container footer-version" $ do
    divClass "ui tiny image" $ do
      elAttr "a" ("href" =: "https://neuron.zettel.page") $ do
        elAttr
          "img"
          ( "src" =: "https://raw.githubusercontent.com/srid/neuron/master/assets/neuron.svg"
              <> "alt" =: "logo"
              <> "title" =: ("Generated by Neuron" <> maybe "" (" " <>) mver)
          )
          blank

-- | Font awesome element
fa :: DomBuilder t m => Text -> m ()
fa k = elClass "i" k blank

actionsNav :: DomBuilder t m => Theme -> Maybe Zettel -> Maybe Text -> NeuronWebT t m ()
actionsNav theme mIndexZettel mEditUrl = elClass "nav" "top-menu" $ do
  divClass ("ui inverted compact neuron icon menu " <> Theme.semanticColor theme) $ do
    forM_ mIndexZettel $ \Zettel {..} ->
      neuronRouteLink (Some $ Route_Zettel zettelSlug) ("class" =: "left item" <> "title" =: "Home") $
        semanticIcon "home"
    neuronRouteLink (Some $ Route_Search Nothing) ("class" =: "left item" <> "title" =: "Search Zettels") $ do
      semanticIcon "search"
    forM_ mEditUrl $ \editUrl -> do
      let attrs = "href" =: editUrl <> "title" =: "Edit this Zettel"
      elAttr "a" ("class" =: "center item" <> attrs) $ do
        semanticIcon "edit"
    neuronRouteLink (Some Route_ZIndex) ("class" =: "right item" <> "title" =: "All Zettels (z-index)") $
      semanticIcon "tree"

style :: Css
style = do
  "body" ? do
    neuronCommonStyle
    ZIndex.style
    ZettelCSS.zettelCss
    QueryView.style
    footerStyle
    navBarStyle
    "i.tree.icon" ? do
      -- Apparently this workarounds the tree icon disappearing or getting
      -- botched at times.
      C.overflow C.hidden
  where
    footerStyle = do
      ".footer-version img" ? do
        C.filter $ C.grayscale $ pct 100
      ".footer-version img:hover" ? do
        C.filter $ C.grayscale $ pct 0
      ".footer-version, .footer-version a, .footer-version a:visited" ? do
        C.color gray
      ".footer-version a" ? do
        C.fontWeight C.bold
      ".footer-version" ? do
        important $ C.marginTop $ em 1
        C.fontSize $ em 0.7
    navBarStyle = do
      "nav.top-menu" ? do
        C.paddingTop $ em 1
        C.paddingBottom $ em 1
        C.justifyContent C.center
        C.textAlign C.center
        "> *" ? do
          C.paddingLeft $ px 0
          C.paddingRight $ px 0
