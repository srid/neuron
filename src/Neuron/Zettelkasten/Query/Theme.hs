{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Query.Theme where

import Control.Monad.Except
import Data.Aeson (ToJSON)
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
  deriving (Eq, Show, Ord, Generic, ToJSON)

type ZettelView = LinkView

data LinkView = LinkView
  { linkViewShowDate :: Bool
  }
  deriving (Eq, Show, Ord, Generic, ToJSON)

instance Default LinkView where
  def = LinkView False

instance Default ZettelsView where
  def = ZettelsView def False

data InvalidLinkView = InvalidLinkView Text
  deriving (Eq, Show)

zettelsViewFromURI :: MonadError InvalidLinkView m => URI.URI -> m (Maybe ZettelsView)
zettelsViewFromURI uri = do
  case (getQueryParam [queryKey|linkTheme|] uri, hasQueryFlag [queryKey|grouped|] uri) of
    (Nothing, False) -> pure Nothing
    (mlinkTheme, grouped) -> do
      let lv = LinkView $ mlinkTheme == Just "withDate"
      pure $ Just $ ZettelsView lv grouped
