{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Frontend.Static.HeadHtml
  ( HeadHtml,
    getHeadHtml,
    getHeadHtmlFromTree,
    renderHeadHtml,
  )
where

import Neuron.CLI.Types (MonadApp (getNotesDir))
import Neuron.Frontend.Route.Data.Types (HeadHtml (..))
import Reflex.Dom.Core
import Reflex.Dom.Pandoc.Raw (RawBuilder, elRawHtml)
import Relude
import System.Directory (doesFileExist)
import qualified System.Directory.Contents as DC
import System.FilePath ((</>))

getHeadHtmlFromTree :: (MonadIO m, MonadApp m) => DC.DirTree FilePath -> m HeadHtml
getHeadHtmlFromTree fileTree = do
  case DC.walkContents "head.html" fileTree of
    Just (DC.DirTree_File fp _) -> do
      headHtmlPath <- getNotesDir <&> (</> fp)
      HeadHtml . Just <$> readFileText headHtmlPath
    _ ->
      pure $ HeadHtml Nothing

getHeadHtml :: (MonadIO m, MonadApp m) => m HeadHtml
getHeadHtml = do
  headHtmlPath <- getNotesDir <&> (</> "head.html")
  liftIO (doesFileExist headHtmlPath) >>= \case
    True -> do
      HeadHtml . Just <$> readFileText headHtmlPath
    False ->
      pure $ HeadHtml Nothing

renderHeadHtml :: (DomBuilder t m, RawBuilder m) => HeadHtml -> m ()
renderHeadHtml (HeadHtml headHtml) = case headHtml of
  Nothing -> do
    -- If the user doesn't specify a head.html, we provide sensible defaults.
    -- For Math support:
    js'
      ("id" =: "MathJax-script" <> "async" =: "")
      "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"
    -- For syntax highlighting:
    css "https://cdnjs.cloudflare.com/ajax/libs/prism/1.23.0/themes/prism-okaidia.min.css"
    js "https://cdnjs.cloudflare.com/ajax/libs/prism/1.23.0/components/prism-core.min.js"
    js "https://cdnjs.cloudflare.com/ajax/libs/prism/1.23.0/plugins/autoloader/prism-autoloader.min.js"
  Just html ->
    elRawHtml html
  where
    css x = elAttr "link" ("rel" =: "stylesheet" <> "href" =: x) blank
    js = js' mempty
    js' attrs x = elAttr "script" (attrs <> "src" =: x) blank
