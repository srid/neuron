{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Query.Theme where

import Data.Aeson (FromJSON, ToJSON)
import Data.Default
import Relude

data ZettelsView = ZettelsView
  { zettelsViewLinkView :: LinkView,
    zettelsViewGroupByTag :: Bool
  }
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

type ZettelView = LinkView

data LinkView
  = LinkView_Default
  | LinkView_ShowDate
  | LinkView_ShowID
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

instance Default LinkView where
  def = LinkView_Default

instance Default ZettelsView where
  def = ZettelsView def False
