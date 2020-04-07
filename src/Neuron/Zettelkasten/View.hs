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
module Neuron.Zettelkasten.View where

import Clay hiding (head, id, ms, reverse, s, type_)
import qualified Clay as C
import Data.Foldable (maximum)
import Data.Tree (Tree (..))
import Lucid
import Neuron.Zettelkasten (neuronVersion)
import Neuron.Zettelkasten.Config
import Neuron.Zettelkasten.Graph
import Neuron.Zettelkasten.ID (ZettelID (..), zettelIDSourceFileName)
import Neuron.Zettelkasten.Link (linkActionExt)
import Neuron.Zettelkasten.Link.Action (LinkTheme (..))
import Neuron.Zettelkasten.Link.View (renderZettelLink)
import Neuron.Zettelkasten.Markdown (neuronMMarkExts)
import Neuron.Zettelkasten.Meta
import Neuron.Zettelkasten.Route
import Neuron.Zettelkasten.Store
import Neuron.Zettelkasten.Type
import Relude
import qualified Rib
import Rib.Extra.CSS (mozillaKbdStyle)
import qualified Rib.Parser.MMark as MMark
import Text.MMark (useExtensions)
import Text.Pandoc.Highlighting (styleToCss, tango)

renderRouteHead :: Monad m => Config -> Route store graph a -> store -> HtmlT m ()
renderRouteHead config r val = do
  meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
  meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
  title_ $ toHtml $ routeTitle config val r
  link_ [rel_ "shortcut icon", href_ "https://raw.githubusercontent.com/srid/neuron/master/assets/logo.ico"]
  case r of
    Route_IndexRedirect ->
      mempty
    _ -> do
      toHtml $ routeOpenGraph config val r
      style_ [type_ "text/css"] $ styleToCss tango

renderRouteBody :: Monad m => Config -> Route store graph a -> (store, graph) -> HtmlT m ()
renderRouteBody config r val = do
  case r of
    Route_ZIndex ->
      renderIndex val
    Route_Zettel zid ->
      renderZettel config val zid
    Route_IndexRedirect ->
      meta_ [httpEquiv_ "Refresh", content_ $ "0; url=" <> Rib.routeUrlRel Route_ZIndex]

renderIndex :: Monad m => (ZettelStore, ZettelGraph) -> HtmlT m ()
renderIndex (store, graph) = do
  h1_ [class_ "header"] $ "Zettel Index"
  div_ [class_ "z-index"] $ do
    -- Cycle detection.
    case topSort graph of
      Left (toList -> cyc) -> div_ [class_ "ui orange segment"] $ do
        h2_ "Cycle detected"
        forM_ cyc $ \zid ->
          li_ $ renderZettelLink LinkTheme_Default store zid
      _ -> mempty
    let clusters = sortMothers $ zettelClusters graph
    p_ $ do
      "There " <> countNounBe "cluster" "clusters" (length clusters) <> " in the Zettelkasten graph. "
      "Each cluster is rendered as a forest, with their roots (mother zettels) highlighted."
    forM_ clusters $ \zids ->
      div_ [class_ "ui piled segment"] $ do
        let forest = dfsForestFrom zids graph
        -- Forest of zettels, beginning with mother vertices.
        ul_ $ renderForest True Nothing LinkTheme_Default store graph forest
  where
    -- Sort clusters with newer mother zettels appearing first.
    sortMothers ms = reverse $ sortOn maximum $ fmap (reverse . sort . toList) ms
    countNounBe noun nounPlural = \case
      1 -> "is 1 " <> noun
      n -> "are " <> show n <> " " <> nounPlural

renderZettel :: forall m. Monad m => Config -> (ZettelStore, ZettelGraph) -> ZettelID -> HtmlT m ()
renderZettel config@Config {..} (store, graph) zid = do
  let Zettel {..} = lookupStore zid store
      mTags = getMeta zettelContent >>= tags
  div_ [class_ "zettel-view"] $ do
    div_ [class_ "ui raised segment"] $ do
      h1_ [class_ "header"] $ toHtml zettelTitle
      let mmarkExts = neuronMMarkExts config
      MMark.render $ useExtensions (linkActionExt store : mmarkExts) zettelContent
    div_ [class_ "ui inverted teal top attached connections segment"] $ do
      renderTags mTags
      div_ [class_ "ui two column grid"] $ do
        div_ [class_ "column"] $ do
          div_ [class_ "ui header"] "Connections"
          let forest = obviateRootUnlessForest zid $ dfsForestFrom [zid] graph
          ul_ $ renderForest True (Just 2) LinkTheme_Simple store graph forest
        div_ [class_ "column"] $ do
          div_ [class_ "ui header"] "Navigate up"
          let forestB = obviateRootUnlessForest zid $ dfsForestBackwards zid graph
          ul_ $ do
            renderForest True Nothing LinkTheme_Simple store graph forestB
    div_ [class_ "ui inverted black bottom attached footer segment"] $ do
      div_ [class_ "ui three column grid"] $ do
        div_ [class_ "center aligned column"] $ do
          a_ [href_ "/", title_ "/"] $ fa "fas fa-home"
        div_ [class_ "center aligned column"] $ do
          whenJust editUrl $ \urlPrefix ->
            a_ [href_ $ urlPrefix <> zettelIDSourceFileName zid, title_ "Edit this Zettel"] $ fa "fas fa-edit"
        div_ [class_ "center aligned column"] $ do
          a_ [href_ (Rib.routeUrlRel Route_ZIndex), title_ "All Zettels (z-index)"] $
            fa "fas fa-tree"
    div_ [class_ "ui one column grid footer-version"] $ do
      div_ [class_ "center aligned column"] $ do
        p_ $ do
          "Generated by "
          a_ [href_ "https://github.com/srid/neuron"] "Neuron"
          " "
          code_ $ toHtml @Text neuronVersion

renderTags :: Monad m => Maybe [Text] -> HtmlT m ()
renderTags = maybe (pure ()) $ \ tags -> do
  div_ [] $ do
    div_ [class_ "ui header"] "Tags"
    ul_ [class_ "tags"] $ do
      forM_ tags $ \ tag -> do
        li_ [class_ "tag"] $ toHtml @Text tag

-- | Font awesome element
fa :: Monad m => Text -> HtmlT m ()
fa k = with i_ [class_ k] mempty

renderForest ::
  Monad m =>
  Bool ->
  Maybe Int ->
  LinkTheme ->
  ZettelStore ->
  ZettelGraph ->
  [Tree ZettelID] ->
  HtmlT m ()
renderForest isRoot maxLevel ltheme s g trees =
  case maxLevel of
    Just 0 -> mempty
    _ -> do
      forM_ (sortForest trees) $ \(Node zid subtrees) ->
        li_ $ do
          let zettelDiv =
                div_
                  [class_ $ bool "" "ui black label" $ ltheme == LinkTheme_Default]
          bool id zettelDiv isRoot $
            renderZettelLink ltheme s zid
          when (ltheme == LinkTheme_Default) $ do
            " "
            case backlinks zid g of
              conns@(_ : _ : _) ->
                -- Has two or more backlinks
                forM_ conns $ \zid2 -> do
                  let z2 = lookupStore zid2 s
                  i_ [class_ "fas fa-link", title_ $ unZettelID zid2 <> " " <> zettelTitle z2] mempty
              _ -> mempty
          when (length subtrees > 0) $ do
            ul_ $ renderForest False ((\n -> n - 1) <$> maxLevel) ltheme s g subtrees
  where
    -- Sort trees so that trees containing the most recent zettel (by ID) come first.
    sortForest = reverse . sortOn maximum

style :: Css
style = do
  let linkColor = C.mediumaquamarine
      linkTitleColor = C.auto
  "span.zettel-link span.zettel-link-idlink a" ? do
    C.fontFamily [] [C.monospace]
    C.fontWeight C.bold
    C.color linkColor
    C.textDecoration C.none
  "span.zettel-link span.zettel-link-idlink a:hover" ? do
    C.backgroundColor linkColor
    C.color C.white
  ".zettel-link .zettel-link-title" ? do
    C.paddingLeft $ em 0.3
    C.fontWeight C.bold
    C.color linkTitleColor
  "div.z-index" ? do
    C.ul ? do
      C.listStyleType C.square
      C.paddingLeft $ em 1.5
  "div.zettel-view" ? do
    C.ul ? do
      C.paddingLeft $ em 1.5
      C.listStyleType C.square
      C.li ? do
        mempty -- C.paddingBottom $ em 1
    C.h1 ? do
      C.paddingTop $ em 0.2
      C.paddingBottom $ em 0.2
      C.textAlign C.center
      C.color C.midnightblue
      C.fontWeight C.bold
      C.backgroundColor C.whitesmoke
    C.h2 ? do
      C.fontColor C.darkslategray
      C.fontWeight C.bold
      C.borderBottom C.solid (px 1) C.steelblue
      C.marginBottom $ em 0.5
    C.h3 ? do
      C.fontColor C.slategray
      C.fontWeight C.bold
      C.margin (px 0) (px 0) (em 0.4) (px 0)
    codeStyle
    blockquoteStyle
    kbd ? mozillaKbdStyle
    "ul.tags" ? do
      C.display C.displayTable
      C.paddingLeft (px 0)
      -- C.fontWeight C.bold
      "li.tag" ? do
        C.display C.block
        C.position C.relative
        C.float C.floatLeft
        C.margin (px 0) (em 0.2) (em 0.5) (em 0.2)
        sym2 C.padding (em 0.3) (em 0.8)
        C.color "#ffffff"
        C.fontSize (em 0.8)
        C.backgroundColor "#2b2c2d"
        sym C.borderRadius (px 3)
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
  where
    codeStyle = do
      C.code ? do
        sym margin auto
        fontSize $ pct 90
      "code, pre, tt" ? do
        fontFamily ["SFMono-Regular", "Menlo", "Monaco", "Consolas", "Liberation Mono", "Courier New"] [monospace]
      pre ? do
        sym padding $ em 0.5
        C.overflow auto
        C.maxWidth $ pct 100
      "div.source-code" ? do
        marginLeft auto
        marginRight auto
        maxWidth $ pct 80
        pre ? do
          backgroundColor "#f8f8f8"
    -- https://css-tricks.com/snippets/css/simple-and-nice-blockquote-styling/
    blockquoteStyle = do
      C.blockquote ? do
        -- TODO: quotes in clay?
        C.backgroundColor "#f9f9f9"
        C.borderLeft C.solid (px 10) "#ccc"
        sym2 C.margin (em 1.5) (px 10)
        sym2 C.padding (em 0.5) (px 10)
        C.p ? do
          C.display C.inline
      "blockquote:before" ? do
        C.color "#ccc"
        C.content C.openQuote
        C.fontSize $ em 4
        C.lineHeight $ em 0.1
        C.marginRight $ em 0.25
        C.verticalAlign $ em $ -0.4
