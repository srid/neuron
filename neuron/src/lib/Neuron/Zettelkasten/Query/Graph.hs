{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Query.Graph where

import Data.Aeson.GADT.TH (deriveJSONGADT)
import Data.Dependent.Sum.Orphans ()
import Data.GADT.Compare.TH (DeriveGEQ (deriveGEq))
import Data.GADT.Show.TH (DeriveGShow (deriveGShow))
import Neuron.Zettelkasten.Connection (Connection)
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.ID (ZettelID)
import Neuron.Zettelkasten.Zettel (Zettel)
import Relude
import Text.Pandoc.Definition (Block)

-- | Like `GraphQuery` but focused on the relationship between zettels.
data GraphQuery r where
  -- | Query the entire graph.
  GraphQuery_Id :: GraphQuery ZettelGraph
  -- | Query backlinks.
  GraphQuery_BacklinksOf ::
    Maybe Connection ->
    ZettelID ->
    GraphQuery [((Connection, [Block]), Zettel)]

deriveJSONGADT ''GraphQuery

deriveGEq ''GraphQuery

deriveGShow ''GraphQuery

deriving instance Show (GraphQuery ZettelGraph)

deriving instance Eq (GraphQuery ZettelGraph)
