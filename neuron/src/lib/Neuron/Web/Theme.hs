{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | HTML & CSS
module Neuron.Web.Theme
  ( Theme (..),
    mkTheme,
    themeCss,
    semanticColor,
    themeIdentifier,
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

themeIdentifier :: Theme -> String
themeIdentifier theme =
  "neuron-theme-default-" <> toString (semanticColor theme)

themeCss :: Css
themeCss = do
  forM_ [minBound .. maxBound] $ \(theme :: Theme) -> do
    let selector = fromString $ "div#" <> themeIdentifier theme
        textColor = withRgb theme rgb
        backgroundColor = withRgb theme rgba 0.1
        backgroundColorLighter = withRgb theme rgba 0.02
    selector ? do
      -- Zettel heading's background color
      ".zettel-content h1" ? do
        C.backgroundColor backgroundColor
      -- Zettel links
      "span.zettel-link-container span.zettel-link a" ? do
        C.color textColor
      "span.zettel-link-container span.zettel-link a:hover" ? do
        C.color C.white
        C.backgroundColor textColor
      -- Bottom stuff
      "nav.bottomPane" ? do
        C.backgroundColor backgroundColorLighter
      -- Zettel footnote's top marging line
      "div#footnotes" ? do
        C.borderTopColor textColor

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
