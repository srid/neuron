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
    zettelCss,
  )
where

import Clay hiding (id, ms, not, object, reverse, s, style, type_)
import qualified Clay as C
import Data.Foldable (maximum)
import Data.TagTree
import Data.Tagged
import qualified Data.Text as T
import Data.Tree (Tree (..))
import qualified Neuron.Web.Theme as Theme
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.Graph (ZettelGraph)
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.ID (zettelIDSourceFileName)
import Neuron.Zettelkasten.Query.Error (QueryError, showQueryError)
import qualified Neuron.Zettelkasten.Query.Eval as Q
import Neuron.Zettelkasten.Query.Theme (LinkView (..))
import qualified Neuron.Zettelkasten.Query.View as Q
import Neuron.Zettelkasten.Query.View (tagUrl, zettelUrl)
import Neuron.Zettelkasten.Zettel
import Reflex.Dom.Core hiding ((&))
import Reflex.Dom.Pandoc
import Relude hiding ((&))
import Text.Pandoc.Definition (Pandoc)

type AutoScroll = Tagged "autoScroll" Bool

renderZettel ::
  PandocBuilder t m =>
  Maybe Text ->
  AutoScroll ->
  (ZettelGraph, PandocZettel) ->
  m [QueryError]
renderZettel editUrl (Tagged autoScroll) (graph, (PandocZettel (z@Zettel {..}, zc))) = do
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
      -- We use -24px (instead of -14px) here so as to not scroll all the way to
      -- title, and as to leave some of the tree visible as "hint" to the user.
      elAttr "div" ("id" =: "zettel-container-anchor" <> "style" =: "position: absolute; top: -24px; left: 0") blank
    divClass "zettel-view" $ do
      errors <- divClass "ui two column grid" $ do
        divClass "one wide tablet only computer only column" $ do
          renderActionsMenu VerticalMenu editUrl (Just z)
        divClass "sixteen wide mobile fifteen wide tablet fifteen wide computer stretched column" $ do
          errors <- renderZettelContent (handleZettelQuery graph) z zc
          divClass "ui bottom attached segment deemphasized" $ do
            divClass "ui two column grid" $ do
              divClass "column" $ do
                whenNotNull (G.backlinks OrdinaryConnection z graph) $ \cfBacklinks -> do
                  elAttr "div" ("class" =: "ui header" <> "title" =: "Zettels that link here, but without branching") $
                    text "More backlinks"
                  el "ul" $ do
                    forM_ cfBacklinks $ \zl ->
                      el "li" $ renderZettelLink Nothing def zl
              divClass "column" $ do
                renderTags zettelTags
          pure errors
      divClass "ui one column grid" $ divClass "mobile only sixteen wide column" $ do
        renderActionsMenu HorizontalMenu editUrl (Just z)
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
  m [QueryError] ->
  URILink ->
  m [QueryError]
handleZettelQuery graph oldRender uriLink = do
  case flip runReaderT (G.getZettels graph) (Q.evalQueryLink uriLink) of
    Left (Left -> e) -> do
      fmap (e :) oldRender <* elError e
    Right Nothing -> do
      oldRender
    Right (Just res) -> do
      -- TODO: This should render in reflex-dom (no via pandoc's builder)
      case Q.buildQueryView res of
        Left (Right -> e) -> do
          fmap (e :) oldRender <* elError e
        Right (Left w) -> do
          elPandocInlines [w]
          pure mempty
        Right (Right w) -> do
          elPandocBlocks [w]
          pure mempty
  where
    elError e =
      elClass "span" "ui left pointing red basic label" $ do
        text $ showQueryError e

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

data MenuOrientation
  = VerticalMenu
  | HorizontalMenu
  deriving (Eq, Show, Ord)

renderActionsMenu :: DomBuilder t m => MenuOrientation -> Maybe Text -> Maybe Zettel -> m ()
renderActionsMenu orient editUrl mzettel = do
  let cls = case orient of
        VerticalMenu -> "ui deemphasized vertical icon menu"
        HorizontalMenu -> "ui deemphasized icon menu"
  divClass cls $ do
    divClass "item" $ do
      elAttr "a" ("href" =: "z-index.html" <> "title" =: "All Zettels (z-index)") $
        fa "fas fa-tree"
    whenJust ((,) <$> mzettel <*> editUrl) $ \(Zettel {..}, urlPrefix) ->
      divClass "item" $ do
        elAttr "a" ("href" =: (urlPrefix <> toText (zettelIDSourceFileName zettelID)) <> "title" =: "Edit this Zettel") $ fa "fas fa-edit"
    divClass "right item" $ do
      elAttr "a" ("href" =: "search.html" <> "title" =: "Search Zettels") $ fa "fas fa-search"
  where
    fa k = elClass "i" k blank

renderZettelContent ::
  forall t m a.
  (PandocBuilder t m, Monoid a) =>
  (m a -> URILink -> m a) ->
  Zettel ->
  Pandoc ->
  m a
renderZettelContent handleLink Zettel {..} doc = do
  elClass "article" "ui raised attached segment zettel-content" $ do
    elClass "h1" "header" $ text zettelTitle
    x <- elPandoc (Config handleLink) doc
    whenJust zettelDay $ \day ->
      elAttr "div" ("class" =: "date" <> "title" =: "Zettel creation date") $ text $ show day
    pure x

renderTags :: DomBuilder t m => [Tag] -> m ()
renderTags tags = do
  forM_ tags $ \t -> do
    -- NOTE(ui): Ideally this should be at the top, not bottom. But putting it at
    -- the top pushes the zettel content down, introducing unnecessary white
    -- space below the title. So we put it at the bottom for now.
    elAttr "span" ("class" =: "ui right ribbon label zettel-tag" <> "title" =: "Tag") $ do
      elAttr
        "a"
        ( "href" =: (tagUrl t)
            <> "class" =: "tag-inner"
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

zettelCss :: Theme.Theme -> Css
zettelCss neuronTheme = do
  zettelCommonCss neuronTheme
  zettelLinkCss neuronTheme
  "div.zettel-view" ? do
    -- This list styling applies both to zettel content, and the rest of the
    -- view (eg: connections pane)
    C.ul ? do
      C.paddingLeft $ em 1.5
      C.listStyleType C.square
      C.li ? do
        mempty -- C.paddingBottom $ em 1
    zettelContentCss neuronTheme
  pureCssTreeDiagram
  ".ui.label.zettel-tag a.tag-inner" ? do
    C.color black
    "a" ? do
      C.color black

-- C.color white

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

lightColor :: Theme.Theme -> Color
lightColor neuronTheme =
  Theme.withRgb neuronTheme C.rgba 0.1

themeColor :: Theme.Theme -> Color
themeColor neuronTheme =
  Theme.withRgb neuronTheme C.rgba 1

zettelContentCss :: Theme.Theme -> Css
zettelContentCss neuronTheme = do
  let linkColor = Theme.withRgb neuronTheme C.rgb
  ".zettel-content" ? do
    -- All of these apply to the zettel content card only.
    "div.date" ? do
      C.textAlign C.center
      C.color C.gray
    C.h1 ? do
      C.paddingTop $ em 0.2
      C.paddingBottom $ em 0.2
      C.textAlign C.center
      C.backgroundColor $ lightColor neuronTheme
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
    definitionListStyle
    blockquoteStyle
  where
    definitionListStyle = do
      C.dl ? do
        C.dt ? do
          C.fontWeight C.bold
        C.dd ? do
          mempty
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

-- https://codepen.io/philippkuehn/pen/QbrOaN
pureCssTreeDiagram :: Css
pureCssTreeDiagram = do
  let cellBorderWidth = px 2
      flipTree = False
      rotateDeg = deg 180
  ".tree.flipped" ? do
    C.transform $ C.rotate rotateDeg
  ".tree" ? do
    C.overflow auto
    when flipTree $ do
      C.transform $ C.rotate rotateDeg
    -- Clay does not support this; doing it inline in div style.
    -- C.transformOrigin $ pct 50
    "ul.root" ? do
      -- Make the tree attach to zettel segment
      C.paddingTop $ px 0
      C.marginTop $ px 0
    "ul" ? do
      C.position relative
      C.padding (em 1) 0 0 0
      C.whiteSpace nowrap
      sym2 C.margin (px 0) auto
      C.textAlign center
      C.after & do
        C.content $ stringContent ""
        C.display C.displayTable
        C.clear both
      C.lastChild & do
        C.paddingBottom $ em 0.1
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
        sym2 C.padding (em 0.2) (em 0.3)
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

zettelCommonCss :: Theme.Theme -> Css
zettelCommonCss neuronTheme = do
  "p" ? do
    C.lineHeight $ pct 150
  "img" ? do
    C.maxWidth $ pct 100 -- Prevents large images from overflowing beyond zettel borders
  ".deemphasized" ? do
    fontSize $ em 0.85
  ".deemphasized:hover" ? do
    opacity 1
    "div.item a:hover" ? important (color $ themeColor neuronTheme)
  ".deemphasized:not(:hover)" ? do
    opacity 0.7
    "span.zettel-link a, div.item a" ? important (color gray)
