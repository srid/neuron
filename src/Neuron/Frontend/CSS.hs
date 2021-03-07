{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Frontend.CSS where

import Clay (Css, em, gray, important, pct, (?))
import qualified Clay as C
import Neuron.Frontend.Common (neuronCommonStyle)
import qualified Neuron.Frontend.Impulse as Impulse
import Neuron.Frontend.Theme (Theme)
import qualified Neuron.Frontend.Zettel.CSS as ZettelCSS
import Relude

neuronStyleForTheme :: Theme -> Css
neuronStyleForTheme theme = do
  "body" ? do
    neuronCommonStyle
    Impulse.style
    ZettelCSS.zettelCss theme
    footerStyle
  where
    footerStyle = do
      ".footer-version img" ? do
        C.filter $ C.grayscale $ pct 100
      ".footer-version img:hover" ? do
        C.filter $ C.grayscale $ pct 0
      ".footer-version, .footer-version a, .footer-version a:visited" ? do
        C.color gray
      ".footer-version a" ? do
        C.fontWeight C.bold
      ".footer-version" ? do
        important $ C.marginTop $ em 1
        C.fontSize $ em 0.7
