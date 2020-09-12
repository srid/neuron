{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Neuron.Web.HeadHtml
  ( HeadHtml,
    getHeadHtml,
    renderHeadHtml,
  )
where

import Development.Shake
import Relude
import Reflex.Dom.Core
import Reflex.Dom.Pandoc (PandocBuilder)
import Reflex.Dom.Pandoc.PandocRaw (PandocRaw (..))
import Rib.Shake (ribInputDir)
import System.FilePath ((</>))
import Text.Pandoc.Definition (Format (..))

newtype HeadHtml = HeadHtml (Maybe Text)

getHeadHtml :: Action HeadHtml
getHeadHtml = do
  headHtmlPath <- ribInputDir <&> (</> "head.html")
  doesFileExist headHtmlPath >>= \case
    True ->
      HeadHtml . Just . toText <$> readFile' headHtmlPath
    False ->
      pure $ HeadHtml Nothing

renderHeadHtml :: PandocBuilder t m => HeadHtml -> m ()
renderHeadHtml (HeadHtml headHtml) = case headHtml of
  Nothing ->
    -- Include the MathJax script if no custom head.html is provided.
    elAttr "script" ("id" =: "MathJax-script" <> "src" =: "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js" <> "async" =: "") blank
  Just html ->
    elPandocRaw (Format "html") html
