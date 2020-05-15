{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Zettel.View
  ( renderZettelContent,
    renderZettelLink,
    zettelCss,
  )
where

import Clay ((?), auto, em, pct, pre, px, sym, sym2)
import Clay (Css)
import qualified Clay as C
import Data.TagTree
import qualified Data.Text as T
import qualified Neuron.Web.Theme as Theme
import Neuron.Zettelkasten.Query.Theme (LinkView (..))
import Neuron.Zettelkasten.Query.View (zettelUrl)
import Neuron.Zettelkasten.Zettel
import Reflex.Dom.Core
import Reflex.Dom.Pandoc.Document
import Relude

renderZettelContent :: PandocBuilder t m => Zettel -> m ()
renderZettelContent Zettel {..} = do
  divClass "ui raised segment zettel-content" $ do
    elClass "h1" "header" $ text zettelTitle
    elPandoc zettelContent
    renderTags zettelTags
    whenJust zettelDay $ \day ->
      elAttr "div" ("class" =: "date" <> "title" =: "Zettel creation date") $ text $ show day

renderTags :: DomBuilder t m => [Tag] -> m ()
renderTags tags = do
  forM_ tags $ \(unTag -> t) -> do
    -- NOTE(ui): Ideally this should be at the top, not bottom. But putting it at
    -- the top pushes the zettel content down, introducing unnecessary white
    -- space below the title. So we put it at the bottom for now.
    elAttr "span" ("class" =: "ui black right ribbon label" <> "title" =: "Tag") $ do
      elAttr
        "a"
        ( "href" =: (tagUrl t)
            <> "title" =: ("See all zettels tagged '" <> t <> "'")
        )
        $ text t
    el "p" blank
  where
    tagUrl s = "/search.html?tag=" <> s

-- | Render a link to an individual zettel.
renderZettelLink :: DomBuilder t m => Maybe LinkView -> Zettel -> m ()
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
