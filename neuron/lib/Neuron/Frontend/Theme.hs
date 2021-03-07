{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | HTML & CSS
module Neuron.Frontend.Theme
  ( Theme (..),
    mkTheme,
    themeCss,
    semanticColor,
    textBackgroundColor,
    textColor,
    titleH1Id,
  )
where

import Clay (Css, rgb, rgba, (?))
import qualified Clay as C
import Data.Text (toLower)
import Relude

-- | Neuron color theme
--
-- Each theme corresponds to the color supported by Semantic UI
-- https://semantic-ui.com/usage/theming.html#sitewide-defaults
data Theme
  = Teal
  | Brown
  | Red
  | Orange
  | Yellow
  | Olive
  | Green
  | Blue
  | Violet
  | Purple
  | Pink
  | Grey
  | Black
  deriving (Eq, Show, Enum, Bounded)

-- | Make Theme from Semantic UI color name
mkTheme :: Text -> Theme
mkTheme s =
  fromMaybe (error $ "Unsupported theme: " <> s) $
    listToMaybe $
      catMaybes $
        flip fmap [minBound .. maxBound] $
          \theme ->
            if s == semanticColor theme
              then Just theme
              else Nothing

-- | Convert Theme to Semantic UI color name
semanticColor :: Theme -> Text
semanticColor = toLower . show @Text

-- | Theme-specific color for notable text
textColor :: Theme -> C.Color
textColor theme = withRgb theme rgb

-- | Theme-specific background color to use on some text
textBackgroundColor :: Theme -> C.Color
textBackgroundColor theme = withRgb theme rgba 0.1

titleH1Id :: Text
titleH1Id = "title-h1"

themeCss :: Theme -> Css
themeCss theme = do
  let backgroundColorLighter = withRgb theme rgba 0.02
  -- Zettel title's background color
  (fromString . toString $ ".zettel-content h1#" <> titleH1Id) ? do
    C.backgroundColor (textBackgroundColor theme)
  -- Bottom stuff
  "nav.bottomPane" ? do
    C.backgroundColor backgroundColorLighter
  -- Zettel footnote's top marging line
  "div#footnotes" ? do
    C.borderTopColor (textColor theme)

withRgb :: Theme -> (Integer -> Integer -> Integer -> a) -> a
withRgb theme f =
  case theme of
    Teal ->
      f 0 181 173
    Brown ->
      f 165 103 63
    Red ->
      f 219 40 40
    Orange ->
      f 242 113 28
    Yellow ->
      f 251 189 8
    Olive ->
      f 181 204 24
    Green ->
      f 33 186 69
    Blue ->
      f 33 133 208
    Violet ->
      f 100 53 201
    Purple ->
      f 163 51 200
    Pink ->
      f 224 57 151
    Grey ->
      f 118 118 118
    Black ->
      f 27 28 29
