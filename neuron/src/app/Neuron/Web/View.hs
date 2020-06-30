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

import Clay ((?), Css, em, gray, important, pct, px)
import qualified Clay as C
import Control.Monad.Except
import Data.Aeson ((.=), object)
import qualified Data.Aeson.Text as Aeson
import qualified Data.Set as Set
import Data.Some
import Data.TagTree (Tag (..))
import Neuron.Config.Type (Config (..))
import Neuron.Web.Common (neuronCommonStyle, neuronFonts)
import Neuron.Web.Manifest (Manifest, renderManifest)
import qualified Neuron.Web.Query.View as QueryView
import Neuron.Web.Route
import Neuron.Web.StructuredData
import Neuron.Web.Theme (Theme)
import qualified Neuron.Web.Theme as Theme
import Neuron.Web.Widget
import qualified Neuron.Web.ZIndex as ZIndex
import qualified Neuron.Web.Zettel.CSS as ZettelCSS
import qualified Neuron.Web.Zettel.View as ZettelView
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.Graph (ZettelGraph)
import Neuron.Zettelkasten.ID (ZettelID, zettelIDSourceFileName)
import Neuron.Zettelkasten.Zettel
import Reflex.Dom.Core hiding ((&))
import Reflex.Dom.Pandoc (PandocBuilder)
import Relude hiding ((&))
import qualified Skylighting.Format.HTML as Skylighting
import qualified Skylighting.Styles as Skylighting

-- | Render the given route
renderRoutePage :: PandocBuilder t m => Text -> Config -> Manifest -> Route a -> (ZettelGraph, a) -> NeuronWebT t m ()
renderRoutePage neuronVersion config manifest r val =
  elAttr "html" ("lang" =: "en") $ do
    el "head" $ do
      renderRouteHead config manifest r val
    el "body" $ do
      renderRouteBody neuronVersion config r val

renderRouteHead :: DomBuilder t m => Config -> Manifest -> Route a -> (ZettelGraph, a) -> m ()
renderRouteHead config manifest route val = do
  elAttr "meta" ("http-equiv" =: "Content-Type" <> "content" =: "text/html; charset=utf-8") blank
  elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1") blank
  el "title" $ text $ routeTitle config (snd val) route
  renderManifest manifest
  case route of
    Route_Redirect _ ->
      blank
    Route_Search {} -> do
      renderCommon
      forM_
        [ "https://cdn.jsdelivr.net/npm/jquery@3.5.0/dist/jquery.min.js",
          "https://cdn.jsdelivr.net/npm/semantic-ui@2.4.2/dist/semantic.min.js",
          "https://cdn.jsdelivr.net/npm/js-search@2.0.0/dist/umd/js-search.min.js"
        ]
        $ \scrpt -> do
          elAttr "script" ("src" =: scrpt) blank
    _ -> do
      renderCommon
      renderStructuredData config route val
      elAttr "style" ("type" =: "text/css") $ text $ toText $ Skylighting.styleToCss Skylighting.tango
  where
    renderCommon = do
      let neuronCss = toText $ C.renderWith C.compact [] $ style config
      elAttr "link" ("rel" =: "stylesheet" <> "href" =: "https://cdn.jsdelivr.net/npm/fomantic-ui@2.8.5/dist/semantic.min.css") blank
      elAttr "style" ("type" =: "text/css") $ text neuronCss
      elLinkGoogleFonts neuronFonts
      when (mathJaxSupport config) $
        elAttr "script" ("id" =: "MathJax-script" <> "src" =: "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js" <> "async" =: "") blank
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
  divClass "ui fluid container" $ do
    let neuronTheme = Theme.mkTheme theme
    case r of
      Route_ZIndex -> do
        actionsNav neuronTheme editUrl Nothing
        divClass "ui text container" $ do
          let zIndexData = ZIndex.buildZIndex g x
          ZIndex.renderZIndex neuronTheme zIndexData
          renderBrandFooter $ Just neuronVersion
        pure mempty
      Route_Search {} -> do
        actionsNav neuronTheme editUrl Nothing
        divClass "ui text container" $ do
          renderSearch g x
          renderBrandFooter $ Just neuronVersion
        pure mempty
      Route_Zettel _ -> do
        -- Don't inject neuron verison in zettel pages, to prevent unnecessary rebuilds when upgrading neuron
        let noVersion = Nothing
        actionsNav neuronTheme editUrl (Just $ either zettelID zettelID x)
        ZettelView.renderZettel (g, x)
          <* renderBrandFooter noVersion
      Route_Redirect _ -> do
        targetUrl <- neuronRouteURL $ Some $ Route_Zettel x
        elAttr "meta" ("http-equiv" =: "Refresh" <> "content" =: ("0; url=" <> targetUrl)) blank

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

renderBrandFooter :: DomBuilder t m => Maybe Text -> m ()
renderBrandFooter mver =
  divClass "ui one column grid footer-version" $ do
    divClass "center aligned column" $ do
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

actionsNav :: DomBuilder t m => Theme -> Maybe Text -> Maybe ZettelID -> NeuronWebT t m ()
actionsNav theme editUrl mzid = elClass "nav" "top-menu" $ do
  divClass ("ui inverted compact neuron icon menu " <> Theme.semanticColor theme) $ do
    neuronRouteLink (Some Route_ZIndex) ("class" =: "left item" <> "title" =: "All Zettels (z-index)") $
      semanticIcon "tree"
    whenJust ((,) <$> mzid <*> editUrl) $ \(zid, urlPrefix) -> do
      let attrs = ("href" =: (urlPrefix <> toText (zettelIDSourceFileName zid)) <> "title" =: "Edit this Zettel")
      elAttr "a" ("class" =: "center item" <> attrs) $ do
        semanticIcon "edit"
    neuronRouteLink (Some Route_Search) ("class" =: "right item" <> "title" =: "Search Zettels") $ do
      semanticIcon "search"

style :: Config -> Css
style Config {..} = do
  let neuronTheme = Theme.mkTheme theme
  "body" ? do
    neuronCommonStyle
    ZIndex.style
    ZettelCSS.zettelCss neuronTheme
    QueryView.style neuronTheme
    footerStyle
    navBarStyle
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
