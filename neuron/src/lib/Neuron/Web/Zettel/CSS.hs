{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Web.Zettel.CSS
  ( zettelCss,
  )
where

import Clay hiding (id, ms, not, object, reverse, s, style, type_)
import qualified Clay as C
import qualified Clay.Media as CM
import qualified Neuron.Web.Theme as Theme
import Neuron.Web.Widget.InvertedTree as IT
import Relude hiding ((&))

zettelCss :: Css
zettelCss = do
  Theme.themeCss
  zettelCommonCss
  C.queryOnly CM.screen [CM.maxWidth $ px 768] $ do
    "div#zettel-container" ? do
      -- Fix too big of margin on mobile.
      C.important $ do
        C.marginLeft $ em 0.4
        C.marginRight $ em 0.4
  "div.zettel-view" ? do
    -- This list styling applies both to zettel content, and the rest of the
    -- view (eg: connections pane)
    C.ul ? do
      C.paddingLeft $ em 1.5
      C.listStyleType C.square
      C.li ? do
        mempty -- C.paddingBottom $ em 1
    zettelContentCss
  IT.style
  ".ui.label.zettel-tag" ? do
    C.color black
    "a" ? do
      C.color black
  -- Backlinks
  "nav.backlinks-container" ? do
    C.important $ C.backgroundColor "#eee"
  "ul.backlinks > li" ? do
    C.paddingBottom $ em 0.4
    C.listStyleType C.disc
  "ul.context-list > li" ? do
    C.listStyleType C.lowerRoman

zettelContentCss :: Css
zettelContentCss = do
  ".pandoc" ? do
    -- GitHub task list
    ".ui.disabled.fitted.checkbox" ? do
      C.marginRight $ em 0.3
      C.verticalAlign C.middle
  ".zettel-content" ? do
    -- All of these apply to the zettel content card only.
    ".metadata" ? do
      C.marginTop $ em 1
      "div.date" ? do
        C.textAlign C.center
        C.color C.gray
    C.h1 ? do
      C.paddingTop $ em 0.2
      C.paddingBottom $ em 0.2
      C.textAlign C.center
    C.h2 ? do
      C.borderBottom C.solid (px 1) C.steelblue
      C.marginBottom $ em 0.5
    C.h3 ? do
      C.margin (px 0) (px 0) (em 0.4) (px 0)
    C.h4 ? do
      C.opacity 0.8
    "div#footnotes" ? do
      C.marginTop $ em 4
      C.borderTopStyle C.groove
      C.borderTopWidth $ px 2
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
  ".zettel-content.raw" ? do
    C.backgroundColor "#ddd"
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

zettelCommonCss :: Css
zettelCommonCss = do
  "p" ? do
    C.lineHeight $ pct 150
  "img" ? do
    C.maxWidth $ pct 100 -- Prevents large images from overflowing beyond zettel borders
  ".deemphasized" ? do
    fontSize $ em 0.85
  ".deemphasized:hover" ? do
    opacity 1
  ".deemphasized:not(:hover)" ? do
    opacity 0.7
    "a" ? important (color gray)
