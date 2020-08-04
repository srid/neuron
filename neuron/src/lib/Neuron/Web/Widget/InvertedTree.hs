{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Web.Widget.InvertedTree
  ( renderInvertedHeadlessTree,
    style,
  )
where

import Clay hiding (id, ms, not, object, reverse, s, style, type_)
import qualified Clay as C
import Data.List (maximum)
import Data.Tree
import Reflex.Dom.Core hiding ((&))
import Relude hiding ((&))

style :: Css
style = pureCssTreeDiagram

renderInvertedHeadlessTree ::
  (DomBuilder t m, Ord a) =>
  -- Element ID
  Text ->
  -- Element Class
  Text ->
  [Tree a] ->
  (a -> m ()) ->
  m ()
renderInvertedHeadlessTree elemId elemCls tree w = do
  let attrs =
        "class" =: ("flipped tree " <> elemCls)
          <> "id" =: elemId
          <> "style" =: "transform-origin: 50%"
  elAttr "nav" attrs $ do
    elClass "ul" "root" $ do
      -- Headless tree will still need a head element (li).
      el "li" $ do
        el "ul" $ do
          renderInvertedForest w tree

renderInvertedForest ::
  (DomBuilder t m, Ord a) =>
  (a -> m ()) ->
  [Tree a] ->
  m ()
renderInvertedForest w trees = do
  forM_ (sortForest trees) $ \(Node x subtrees) ->
    el "li" $ do
      divClass "forest-link" $
        w x
      when (length subtrees > 0) $ do
        el "ul" $ renderInvertedForest w subtrees
  where
    -- Sort trees so that trees containing the most recent zettel (by ID) come first.
    sortForest = reverse . sortOn maximum

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
      forM_ [C.before, C.after] $ \sel ->
        sel & do
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
        forM_ [C.after, C.before] $ \sel ->
          sel & do
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
