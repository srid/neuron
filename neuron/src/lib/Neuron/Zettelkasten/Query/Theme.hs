{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Neuron.Zettelkasten.Query.Theme where

import Lens.Micro.Platform (over, _head)
import Data.Char (toLower)
import Data.Aeson (FromJSON, ToJSON)
import Data.Default
import Relude

newtype GroupByTag = GroupByTag { unGroupByTag :: Bool }
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

data Columns where
  Zero     :: Columns
  One      :: Columns
  Two      :: Columns
  Three    :: Columns
  Four     :: Columns
  Five     :: Columns
  Six      :: Columns
  Seven    :: Columns
  Eight    :: Columns
  Nine     :: Columns
  Ten      :: Columns
  Eleven   :: Columns
  Twelve   :: Columns
  Thirteen :: Columns
  Fourteen :: Columns
  Fifteen  :: Columns
  Sixteen  :: Columns
  deriving (Eq, Show, Ord, Enum, Bounded, Generic, ToJSON, FromJSON)

class Semantic a where
  showSemantic :: a -> Text

instance Semantic Columns where
  showSemantic x = over _head toLower (show x)

class ToColumns a where
  toColumns :: a -> Columns

instance ToColumns Natural where
  toColumns n
    | n <= (fromIntegral . fromEnum) (maxBound :: Columns) = toEnum $ fromIntegral n
    | otherwise = def

data ZettelsViewAttr where
  ZettelsViewAttr :: { zettelsViewAttr_LinkView :: LinkView
                     , zettelsViewAttr_GroupByTag :: GroupByTag
                     } -> ZettelsViewAttr
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

data ZettelsView where
  ZettlesView_List :: { zettelsView_List_Attr :: ZettelsViewAttr
                      } -> ZettelsView
  ZettlesView_Tabular :: { zettelsView_Tab_Attr :: ZettelsViewAttr
                         ,  zettelsView_Tab_Columns :: Columns
                         } -> ZettelsView
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

type ZettelView = LinkView

newtype LinkView = LinkView
  { linkViewShowDate :: Bool
  }
  deriving (Eq, Show, Ord, Generic, ToJSON, FromJSON)

instance Default LinkView where
  def = LinkView False

instance Default Columns where
  def = Two

instance Default GroupByTag where
  def = coerce False

instance Default ZettelsViewAttr where
  def = ZettelsViewAttr def def

instance Default ZettelsView where
  def = ZettlesView_List def
