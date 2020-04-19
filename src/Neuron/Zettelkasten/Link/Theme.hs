{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Link.Theme where

import Control.Monad.Except
import Relude
import qualified Text.URI as URI

data ZettelsView = ZettelsView
  { zettelsViewLinkTheme :: LinkTheme,
    zettelsViewGroupByTag :: Bool
  }
  deriving (Eq, Show, Ord)

type ZettelView = LinkTheme

data LinkTheme
  = LinkTheme_Default
  | LinkTheme_Simple
  | LinkTheme_WithDate
  deriving (Eq, Show, Ord)

zettelsViewFromURI :: MonadError Text m => URI.URI -> m ZettelsView
zettelsViewFromURI uri =
  ZettelsView
    <$> linkThemeFromURI uri
    <*> groupByTag
  where
    groupByTag = do
      x <- fmap (listToMaybe . catMaybes) $ flip traverse (URI.uriQuery uri) $ \case
        URI.QueryFlag (URI.unRText -> key) | key == "grouped" ->
          pure $ Just True
        _ -> pure Nothing
      pure $ fromMaybe False x

linkThemeFromURI :: MonadError Text m => URI.URI -> m LinkTheme
linkThemeFromURI uri = do
  ltm <- fmap (listToMaybe . catMaybes) $ flip traverse (URI.uriQuery uri) $ \case
    URI.QueryFlag _ -> pure Nothing
    URI.QueryParam (URI.unRText -> key) (URI.unRText -> val) ->
      case key of
        "linkTheme" ->
          case val of
            "default" -> pure $ Just LinkTheme_Default
            "simple" -> pure $ Just LinkTheme_Simple
            "withDate" -> pure $ Just LinkTheme_WithDate
            _ -> throwError $ "Unknown link theme: " <> val
        _ -> pure Nothing
  pure $ fromMaybe LinkTheme_Default ltm
