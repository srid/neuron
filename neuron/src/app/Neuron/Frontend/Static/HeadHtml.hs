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
  Nothing ->
    -- Include the MathJax script if no custom head.html is provided.
    elAttr "script" ("id" =: "MathJax-script" <> "src" =: "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js" <> "async" =: "") blank
  Just html ->
    elRawHtml html
