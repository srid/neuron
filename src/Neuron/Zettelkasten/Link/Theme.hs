{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Link.Theme where

import Relude
import qualified Text.URI as URI

data LinkTheme
  = LinkTheme_Default
  | LinkTheme_Simple
  | LinkTheme_WithDate
  deriving (Eq, Show, Ord)

-- TODO: MonadError
linkThemeFromURI :: URI.URI -> LinkTheme
linkThemeFromURI uri =
  fromMaybe LinkTheme_Default $ listToMaybe $ flip mapMaybe (URI.uriQuery uri) $ \case
    URI.QueryFlag _ -> Nothing
    URI.QueryParam (URI.unRText -> key) (URI.unRText -> val) ->
      case key of
        "linkTheme" ->
          case val of
            "default" -> Just LinkTheme_Default
            "simple" -> Just LinkTheme_Simple
            "withDate" -> Just LinkTheme_WithDate
            _ -> error $ "Unknown link theme: " <> val
        _ -> Nothing
