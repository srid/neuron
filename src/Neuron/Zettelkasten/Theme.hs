{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | HTML & CSS
module Neuron.Zettelkasten.Theme where

import Data.Text (toLower)
import Relude

-- | Neuron color theme
--
-- Each theme corresponds to the color supported by Semantic UI
-- https://semantic-ui.com/usage/theming.html#sitewide-defaults
-- @red            : #B03060;
-- @orange         : #FE9A76;
-- @yellow         : #FFD700;
-- @olive          : #32CD32;
-- @green          : #016936;
-- @teal           : #008080;
-- @blue           : #0E6EB8;
-- @violet         : #EE82EE;
-- @purple         : #B413EC;
-- @pink           : #FF1493;
-- @brown          : #A52A2A;
-- @grey           : #A0A0A0;
-- @black          : #000000;
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
      f 0 128 128
    Brown ->
      f 165 42 42
    Red ->
      f 176 48 96
    Orange ->
      f 254 154 118
    Yellow ->
      f 255 215 0
    Olive ->
      f 50 205 50
    Green ->
      f 1 105 54
    Blue ->
      f 14 110 184
    Violet ->
      f 238 130 238
    Purple ->
      f 180 19 236
    Pink ->
      f 255 20 147
    Grey ->
      f 160 160 160
    Black ->
      f 0 0 0

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
