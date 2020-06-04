{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Query.Graph where

import Data.Aeson
import Data.Aeson.GADT.TH
import Data.Dependent.Sum.Orphans ()
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.Graph.Type
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Zettel
import Relude

-- | Like `GraphQuery` but focused on the relationship between zettels.
data GraphQuery r where
  -- | Query the entire graph.
  GraphQuery_Id :: GraphQuery ZettelGraph
  -- | Query backlinks.
  GraphQuery_BacklinksOf ::
    Maybe Connection ->
    ZettelID ->
    GraphQuery [(Connection, Zettel)]

deriveJSONGADT ''GraphQuery

deriveGEq ''GraphQuery

deriveGShow ''GraphQuery

deriving instance Show (GraphQuery ZettelGraph)

deriving instance Eq (GraphQuery ZettelGraph)
