{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Graph.Labelled.Type where

import qualified Algebra.Graph.Labelled.AdjacencyMap as LAM
import Data.Aeson
import qualified Data.Map.Strict as Map
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
  deriving (Generic)

deriving instance (Eq e, Eq v, Eq (VertexID v)) => Eq (LabelledGraph v e)

deriving instance
  ( Ord e,
    Show e,
    Show v,
    Ord (VertexID v),
    Show (VertexID v)
  ) =>
  Show (LabelledGraph v e)

instance (ToJSONKey (VertexID v), ToJSON v, ToJSON e) => ToJSON (LabelledGraph v e) where
  toJSON (LabelledGraph g vs) =
    toJSON $
      object
        [ "adjacencyMap" .= LAM.adjacencyMap g,
          "vertices" .= vs
        ]

instance (FromJSONKey (VertexID v), Ord (VertexID v), Eq e, Monoid e, FromJSON v, FromJSON e) => FromJSON (LabelledGraph v e) where
  parseJSON =
    withObject "LabelledGraph" $ \lg ->
      LabelledGraph
        <$> (fmap mkGraph $ lg .: "adjacencyMap")
        <*> lg .: "vertices"
    where
      mkGraph = LAM.fromAdjacencyMaps . Map.toList
