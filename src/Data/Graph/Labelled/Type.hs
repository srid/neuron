{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Graph.Labelled.Type
  ( -- * Graph type
    -- TODO: don't expose internals
    LabelledGraph(..),
    IsVertex (..),
  )
where

import qualified Algebra.Graph.Labelled.AdjacencyMap as LAM
import Relude

class IsVertex v where
  type VertexID v
  vertexID :: v -> VertexID v

-- | An edge and vertex labelled graph
--
-- TODO: Vertex labelling
data LabelledGraph v e = LabelledGraph
  { graph :: LAM.AdjacencyMap e (VertexID v),
    vertices :: Map (VertexID v) v
  }
