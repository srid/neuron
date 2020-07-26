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
  ".ui.container" ? do
    -- Override Semantic UI's font
    C.important $ C.fontFamily [bodyFont] [C.serif]
  "h1, h2, h3, h4, h5, h6, .ui.header, .headerFont" ? do
    C.important $ C.fontFamily [headerFont] [C.sansSerif]
  "code, pre, tt, .monoFont" ? do
    C.important $ C.fontFamily [monoFont, "SFMono-Regular", "Menlo", "Monaco", "Consolas", "Liberation Mono", "Courier New"] [C.monospace]

neuronFonts :: [Text]
neuronFonts = [headerFont, bodyFont, monoFont]

headerFont :: Text
headerFont = "Kanit"

bodyFont :: Text
bodyFont = "Ubuntu"

monoFont :: Text
monoFont = "Roboto Mono"
