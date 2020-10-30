{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Graph.Type
  ( -- * Graph type
    ZettelGraph,
  )
where

import Data.Graph.Labelled
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.Zettel
import Relude
import Text.Pandoc.Definition (Block)

-- | The Zettelkasten graph
--
-- Edges are labelled with `Connection`; Maybe is used to provide the
-- `Algebra.Graph.Label.zero` value for the edge label, which is `Nothing` in
-- our case, and is effectively the same as there not being an edge between
-- those vertices.
type ZettelGraph = LabelledGraph Zettel (Maybe (Connection, [Block]))
