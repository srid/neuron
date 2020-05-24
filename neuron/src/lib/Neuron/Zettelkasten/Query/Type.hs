{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Query.Type where

import Data.Aeson
import Data.Aeson.GADT.TH
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Data.TagTree (Tag, TagPattern (..))
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.Graph.Type
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Query.Theme
import Neuron.Zettelkasten.Zettel
import Relude

-- | ZettelQuery queries individual zettels.
--
-- It does not care about the relationship *between* those zettels; for that use `GraphQuery`.
data ZettelQuery r where
  ZettelQuery_ZettelByID :: ZettelID -> Maybe Connection -> ZettelQuery (Maybe Zettel)
  ZettelQuery_ZettelsByTag :: [TagPattern] -> Maybe Connection -> ZettelsView -> ZettelQuery [Zettel]
  ZettelQuery_Tags :: [TagPattern] -> ZettelQuery (Map Tag Natural)

-- | Like `GraphQuery` but focused on the relationship between zettels.
data GraphQuery r where
  -- | Query the entire graph.
  GraphQuery_Id :: GraphQuery ZettelGraph

deriveJSONGADT ''ZettelQuery

deriveGEq ''ZettelQuery

deriveGShow ''ZettelQuery

deriving instance Show (ZettelQuery (Maybe Zettel))

deriving instance Show (ZettelQuery [Zettel])

deriving instance Show (ZettelQuery (Map Tag Natural))

deriving instance Eq (ZettelQuery (Maybe Zettel))

deriving instance Eq (ZettelQuery [Zettel])

deriving instance Eq (ZettelQuery (Map Tag Natural))

deriveJSONGADT ''GraphQuery

deriveGEq ''GraphQuery

deriveGShow ''GraphQuery

deriving instance Show (GraphQuery ZettelGraph)

deriving instance Eq (GraphQuery ZettelGraph)
