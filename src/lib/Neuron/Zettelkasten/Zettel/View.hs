{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Zettel.View
  ( renderZettelContent,
    renderZettelLink,
    renderZettel,
    renderFooter,
    zettelCss,
    zettelLinkCss,
  )
where

import Clay ((?), auto, em, pct, pre, px, sym, sym2)
import Clay (Css)
import qualified Clay as C
import Data.Foldable (maximum)
import Data.TagTree
import Data.Tagged
import qualified Data.Text as T
import Data.Tree (Tree (..))
import qualified Neuron.Web.Theme as Theme
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.Error (NeuronError (..))
import Neuron.Zettelkasten.Graph (ZettelGraph)
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.ID (ZettelID (..), zettelIDSourceFileName)
import qualified Neuron.Zettelkasten.Query.Eval as Q
import Neuron.Zettelkasten.Query.Theme (LinkView (..))
import qualified Neuron.Zettelkasten.Query.View as Q
import Neuron.Zettelkasten.Query.View (tagUrl, zettelUrl)
import Neuron.Zettelkasten.Zettel
import Reflex.Dom.Core
import Reflex.Dom.Pandoc
import Relude

type AutoScroll = Tagged "autoScroll" Bool

renderZettel :: PandocBuilder t m => Maybe Text -> AutoScroll -> (ZettelGraph, Zettel) -> m [NeuronError]
renderZettel editUrl (Tagged autoScroll) (graph, z@Zettel {..}) = do
  let upTree = G.backlinkForest Folgezettel z graph
  whenNotNull upTree $ \_ -> do
    let attrs =
          "class" =: "flipped tree deemphasized"
            <> "id" =: "zettel-uptree"
            <> "style" =: "transform-origin: 50%"
    elAttr "div" attrs $ do
      elClass "ul" "root" $ do
        el "li" $ do
          el "ul" $ do
            renderUplinkForest (\z2 -> G.getConnection z z2 graph) upTree
  -- Main content
  errors <- elAttr "div" ("class" =: "ui text container" <> "id" =: "zettel-container" <> "style" =: "position: relative") $ do
    when autoScroll $ do
      -- zettel-container-anchor is a trick used by the scrollIntoView JS below
      -- cf. https://stackoverflow.com/a/49968820/55246
      elAttr "div" ("id" =: "zettel-container-anchor" <> "style" =: "position: absolute; top: -14px; left: 0") blank
    divClass "zettel-view" $ do
      errors <- renderZettelContent (handleZettelQuery graph zettelID) z
      whenNotNull (G.backlinks OrdinaryConnection z graph) $ \cfBacklinks -> do
        divClass "ui attached segment deemphasized" $ do
          elAttr "div" ("class" =: "ui header" <> "title" =: "Zettels that link here, but without branching") $
            text "More backlinks"
          el "ul" $ do
            forM_ cfBacklinks $ \zl ->
              el "li" $ renderZettelLink Nothing def zl
      renderFooter editUrl graph (Just z)
      pure errors
  -- Because the tree above can be pretty large, we scroll past it
  -- automatically when the page loads.
  -- FIXME: This may not scroll sufficiently if the images in the zettel haven't
  -- loaded (thus the browser doesn't known the final height yet.)
  when (autoScroll && not (null upTree)) $ do
    whenNotNull upTree $ \_ -> do
      el "script" $ text $
        "document.getElementById(\"zettel-container-anchor\").scrollIntoView({behavior: \"smooth\", block: \"start\"});"
  pure errors

handleZettelQuery ::
  (PandocRawConstraints m, DomBuilder t m, PandocRaw m) =>
  ZettelGraph ->
  ZettelID ->
  m [NeuronError] ->
  URILink ->
  m [NeuronError]
handleZettelQuery graph zettelID oldRender uriLink = do
  case flip runReaderT (G.getZettels graph) (Q.evalQueryLink uriLink) of
    Left (NeuronError_BadQuery zettelID . Left -> e) -> do
      -- TODO: show the error in terminal, or better report it correctly.
      -- see github issue.
      divClass "ui error message" $ do
        text $ show e
      fmap (e :) oldRender
    Right Nothing -> do
      oldRender
    Right (Just res) -> do
      -- TODO: This should render in reflex-dom (no via pandoc's builder)
      case Q.buildQueryView res of
        Left (NeuronError_BadQuery zettelID . Right -> e) -> do
          divClass "ui error message" $ do
            text $ show e
          fmap (e :) oldRender
        Right (Left w) -> do
          elPandocInlines [w]
          pure mempty
        Right (Right w) -> do
          elPandocBlocks [w]
          pure mempty

renderUplinkForest ::
  DomBuilder t m =>
  (Zettel -> Maybe Connection) ->
  [Tree Zettel] ->
  m ()
renderUplinkForest getConn trees = do
  forM_ (sortForest trees) $ \(Node zettel subtrees) ->
    el "li" $ do
      divClass "forest-link" $
        renderZettelLink (getConn zettel) def zettel
      when (length subtrees > 0) $ do
        el "ul" $ renderUplinkForest getConn subtrees
  where
    -- Sort trees so that trees containing the most recent zettel (by ID) come first.
    sortForest = reverse . sortOn maximum

renderFooter :: DomBuilder t m => Maybe Text -> ZettelGraph -> Maybe Zettel -> m ()
renderFooter editUrl graph mzettel = do
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
        elAttr "a" ("href" =: "search.html" <> "title" =: "Search Zettels") $ fa "fas fa-search"
      divClass "center aligned column" $ do
        elAttr "a" ("href" =: "z-index.html" <> "title" =: "All Zettels (z-index)") $
          fa "fas fa-tree"
  where
    fa k = elClass "i" k blank

renderZettelContent ::
  forall t m a.
  (PandocBuilder t m, Monoid a) =>
  (m a -> URILink -> m a) ->
  Zettel ->
  m a
renderZettelContent handleLink Zettel {..} = do
  divClass "ui raised top attached segment zettel-content" $ do
    elClass "h1" "header" $ text zettelTitle
    x <- elPandoc (Config handleLink) zettelContent
    renderTags zettelTags
    whenJust zettelDay $ \day ->
      elAttr "div" ("class" =: "date" <> "title" =: "Zettel creation date") $ text $ show day
    pure x

renderTags :: DomBuilder t m => [Tag] -> m ()
renderTags tags = do
  forM_ tags $ \t -> do
    -- NOTE(ui): Ideally this should be at the top, not bottom. But putting it at
    -- the top pushes the zettel content down, introducing unnecessary white
    -- space below the title. So we put it at the bottom for now.
    elAttr "span" ("class" =: "ui black right ribbon label" <> "title" =: "Tag") $ do
      elAttr
        "a"
        ( "href" =: (tagUrl t)
            <> "title" =: ("See all zettels tagged '" <> unTag t <> "'")
        )
        $ text
        $ unTag t
    el "p" blank

-- | Render a link to an individual zettel.
renderZettelLink :: DomBuilder t m => Maybe Connection -> Maybe LinkView -> Zettel -> m ()
renderZettelLink conn (fromMaybe def -> LinkView {..}) Zettel {..} = do
  let connClass = maybe "" show conn
      mextra =
        if linkViewShowDate
          then case zettelDay of
            Just day ->
              Just $ show @Text day
            Nothing ->
              Nothing
          else Nothing
  elClass "span" ("zettel-link-container " <> connClass) $ do
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

zettelLinkCss :: Theme.Theme -> Css
zettelLinkCss neuronTheme = do
  let linkColor = Theme.withRgb neuronTheme C.rgb
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
  "span.zettel-link-container.folgezettel::after" ? do
    C.paddingLeft $ em 0.3
    C.content $ C.stringContent "ᛦ"
  "[data-tooltip]:after" ? do
    C.fontSize $ em 0.7

zettelCss :: Theme.Theme -> Css
zettelCss neuronTheme = do
  let linkColor = Theme.withRgb neuronTheme C.rgb
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
    -- End of div.zettel-content
    codeStyle
    blockquoteStyle
  where
    codeStyle = do
      C.code ? do
        sym C.margin auto
        C.fontSize $ pct 100
      -- This pretty much selects inline code elements
      "p code, li code, ol code" ? do
        sym C.padding $ em 0.2
        C.backgroundColor "#f8f8f8"
      -- This selects block code elements
      pre ? do
        sym C.padding $ em 0.5
        C.overflow auto
        C.maxWidth $ pct 100
      "div.pandoc-code" ? do
        C.marginLeft auto
        C.marginRight auto
        pre ? do
          C.backgroundColor "#f8f8f8"
    -- https://css-tricks.com/snippets/css/simple-and-nice-blockquote-styling/
    blockquoteStyle =
      C.blockquote ? do
        C.backgroundColor "#f9f9f9"
        C.borderLeft C.solid (px 10) "#ccc"
        sym2 C.margin (em 1.5) (px 0)
        sym2 C.padding (em 0.5) (px 10)
