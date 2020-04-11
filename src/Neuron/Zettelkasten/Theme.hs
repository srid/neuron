{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | HTML & CSS
module Neuron.Zettelkasten.Theme where

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

-- | Convert Theme to Semantic UI color name
semanticColor :: Theme -> Text
semanticColor = toLower . show @Text

-- | Make Theme from Semantic UI color name
mkTheme :: Text -> Theme
mkTheme s =
  fromMaybe (error $ "Unsupported theme: " <> s)
    $ listToMaybe
    $ catMaybes
    $ flip fmap [minBound .. maxBound]
    $ \theme ->
      if s == semanticColor theme
        then Just theme
        else Nothing

defaultTheme :: Theme
defaultTheme = Teal
