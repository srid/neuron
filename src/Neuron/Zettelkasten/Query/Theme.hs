{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Query.Theme where

import Control.Monad.Except
import Data.Default
import Data.TagTree (Tag)
import Neuron.Zettelkasten.Zettel
import Relude
import qualified Text.URI as URI
import Text.URI.QQ (queryKey)
import Text.URI.Util (getQueryParam, hasQueryFlag)

type family QueryTheme q

type instance QueryTheme (Maybe Zettel) = ZettelView

type instance QueryTheme [Zettel] = ZettelsView

type instance QueryTheme (Map Tag Natural) = ()

data ZettelsView = ZettelsView
  { zettelsViewLinkView :: LinkView,
    zettelsViewGroupByTag :: Bool
  }
  deriving (Eq, Show, Ord)

type ZettelView = LinkView

data LinkView = LinkView
  { linkViewShowDate :: Bool
  }
  deriving (Eq, Show, Ord)

instance Default LinkView where
  def = LinkView False

data InvalidLinkView = InvalidLinkView Text
  deriving (Eq, Show)

zettelsViewFromURI :: MonadError InvalidLinkView m => URI.URI -> m ZettelsView
zettelsViewFromURI uri =
  ZettelsView
    <$> linkThemeFromURI uri
    <*> pure (hasQueryFlag [queryKey|grouped|] uri)

linkThemeFromURI :: MonadError InvalidLinkView m => URI.URI -> m LinkView
linkThemeFromURI uri = do
  let showDate = maybe False (bool False True . (== "withDate")) $ getQueryParam [queryKey|linkTheme|] uri
  pure $ def {linkViewShowDate = showDate}
