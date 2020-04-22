{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Graph.Labelled.Type
  ( -- * Graph type
    -- TODO: don't expose internals
    LabelledGraph (..),
    Vertex (..),
  )
where

import qualified Algebra.Graph.Labelled.AdjacencyMap as LAM
import Relude

-- | Instances of this class can be used as a vertex in a graph.
class Vertex v where
  type VertexID v :: Type

  -- | Get the vertex ID associated with this vertex.
  --
  -- This relation is expected to be bijective.
  vertexID :: v -> VertexID v

-- | An edge and vertex labelled graph
--
-- The `v` must be an instance of `Vertex`; as such `v` is considered the vertex
-- label, with the actual vertex (bijectively) derived from it.
data LabelledGraph v e = LabelledGraph
  { graph :: LAM.AdjacencyMap e (VertexID v),
    vertices :: Map (VertexID v) v
  }
