{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Web.Common where

import Clay ((?), Css)
import qualified Clay as C
import Relude

neuronCommonStyle :: Css
neuronCommonStyle = do
  C.important $ C.backgroundColor "#eee"
  C.important $ C.fontFamily [bodyFont] [C.serif]
  "h1, h2, h3, h4, h5, h6, .ui.header, .headerFont" ? do
    C.important $ C.fontFamily [headerFont] [C.sansSerif]
  "code, pre, tt, .monoFont" ? do
    C.important $ C.fontFamily [monoFont, "SFMono-Regular", "Menlo", "Monaco", "Consolas", "Liberation Mono", "Courier New"] [C.monospace]

neuronFonts :: [Text]
neuronFonts = [headerFont, bodyFont, monoFont]

headerFont :: Text
headerFont = "DM Serif Text"

bodyFont :: Text
bodyFont = "DM Sans"

monoFont :: Text
monoFont = "DM Mono"
