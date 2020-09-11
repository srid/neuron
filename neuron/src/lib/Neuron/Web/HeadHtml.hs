{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Neuron.Web.HeadHtml
  ( HeadHtml,
    getHeadHtml,
    renderHeadHtmlOr,
  )
where

import Development.Shake
import Relude
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

renderHeadHtmlOr :: PandocBuilder t m => HeadHtml -> m () -> m ()
renderHeadHtmlOr (HeadHtml headHtml) other = case headHtml of
  Nothing -> other
  Just html -> elPandocRaw (Format "html") html
