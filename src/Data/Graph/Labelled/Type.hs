{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Graph.Labelled.Type
  ( -- * Graph type
    LabelledGraph,
  )
where

import qualified Algebra.Graph.Labelled.AdjacencyMap as LAM

-- | An edge and vertex labelled graph
--
-- TODO: Vertex labelling
type LabelledGraph v edgeLabel = LAM.AdjacencyMap edgeLabel v
