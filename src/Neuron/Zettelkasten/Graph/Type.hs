{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Graph.Type
  ( -- * Graph type
    ZettelGraph,

    -- * Querying
    findVertex,
    getVertices,
  )
where

import Data.Graph.Labelled.Algorithm
import Data.Graph.Labelled.Type
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Zettel

-- | The Zettelkasten graph
type ZettelGraph = LabelledGraph Zettel [Connection]
