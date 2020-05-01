{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Query.Theme where

import Data.Aeson (ToJSON)
import Data.Default
import Data.TagTree (Tag)
import Neuron.Zettelkasten.Zettel
import Relude

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
