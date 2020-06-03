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

import Clay hiding (Plain, id, ms, name, object, reverse, s, style, type_)
import qualified Clay as C
import Control.Monad.Except
import Data.Aeson ((.=), object)
import qualified Data.Aeson.Text as Aeson
import Data.FileEmbed (embedStringFile)
import qualified Data.Set as Set
import Data.Some
import Data.TagTree (Tag (..))
import Neuron.Config
import Neuron.Version (neuronVersion)
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
import Neuron.Zettelkasten.ID (zettelIDSourceFileName)
import Neuron.Zettelkasten.Zettel
import Reflex.Dom.Core hiding ((&))
import Reflex.Dom.Pandoc (PandocBuilder)
import Relude hiding ((&))
import Rib (routeUrlRel)
import qualified Skylighting.Format.HTML as Skylighting
import qualified Skylighting.Styles as Skylighting

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
      renderStructuredData config route val
      elAttr "style" ("type" =: "text/css") $ text $ toText $ Skylighting.styleToCss Skylighting.tango
  where
    routeTitle :: Config -> a -> Route a -> Text
    routeTitle Config {..} v =
      withSuffix siteTitle . routeTitle' v
      where
        withSuffix suffix x =
          if x == suffix
            then x
            else x <> " - " <> suffix

renderRouteBody :: PandocBuilder t m => Config -> Route a -> (ZettelGraph, a) -> NeuronWebT t m (RouteError a)
renderRouteBody Config {..} r (g, x) = do
  let neuronTheme = Theme.mkTheme theme
  case r of
    Route_ZIndex -> do
      actionsNav neuronTheme editUrl Nothing
      divClass "ui text container" $ do
        ZIndex.renderZIndex neuronTheme g x
        renderBrandFooter
      pure mempty
    Route_Search {} -> do
      actionsNav neuronTheme editUrl Nothing
      divClass "ui text container" $ do
        renderSearch g
        renderBrandFooter
      pure mempty
    Route_Zettel _ -> do
      actionsNav neuronTheme editUrl (Just $ fst $ unPandocZettel x)
      ZettelView.renderZettel (g, x)
        <* renderBrandFooter
    Route_Redirect _ -> do
      elAttr "meta" ("http-equiv" =: "Refresh" <> "content" =: ("0; url=" <> (Rib.routeUrlRel $ Route_Zettel x))) blank
      pure mempty

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

actionsNav :: DomBuilder t m => Theme -> Maybe Text -> Maybe Zettel -> NeuronWebT t m ()
actionsNav theme editUrl mzettel = elClass "nav" "top-menu" $ do
  divClass ("ui inverted compact neuron icon menu " <> Theme.semanticColor theme) $ do
    neuronRouteLink (Some Route_ZIndex) ("class" =: "left item" <> "title" =: "All Zettels (z-index)") $
      semanticIcon "tree"
    whenJust ((,) <$> mzettel <*> editUrl) $ \(Zettel {..}, urlPrefix) -> do
      let attrs = ("href" =: (urlPrefix <> toText (zettelIDSourceFileName zettelID)) <> "title" =: "Edit this Zettel")
      elAttr "a" ("class" =: "center item" <> attrs) $ do
        semanticIcon "edit"
    neuronRouteLink (Some Route_Search) ("class" =: "right item" <> "title" =: "Search Zettels") $ do
      semanticIcon "search"

style :: Config -> Css
style Config {..} = do
  let neuronTheme = Theme.mkTheme theme
  "div.z-index" ? do
    C.ul ? do
      C.listStyleType C.square
      C.paddingLeft $ em 1.5
  ZettelCSS.zettelCss neuronTheme
  QueryView.style neuronTheme
  ".footer-version, .footer-version a, .footer-version a:visited" ? do
    C.color gray
  ".footer-version a" ? do
    C.fontWeight C.bold
  ".footer-version" ? do
    C.marginTop $ em 1
    C.fontSize $ em 0.7
  "nav.top-menu" ? do
    C.paddingTop $ em 1
    C.paddingBottom $ em 1
    C.justifyContent C.center
    C.textAlign C.center
    "> *" ? do
      C.paddingLeft $ px 0
      C.paddingRight $ px 0
