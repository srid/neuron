{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.Graph.Labelled.AlgorithmSpec
  ( spec,
  )
where

import Data.Graph.Labelled
import Relude
import Test.Hspec

newtype SimpleVertex = SimpleVertex String
  deriving (Show, Eq, Ord, IsString)

instance Vertex SimpleVertex where
  type VertexID SimpleVertex = String
  vertexID (SimpleVertex v) = v

spec :: Spec
spec = do
  let mkG = mkGraphFrom @(Maybe ()) @SimpleVertex
      e = Just ()
  describe "cluster" $ do
    it "detects 1-graph cluster" $ do
      clusters (mkG ["a"] []) `shouldBe` ["a" :| []]
    it "detects 2-graph clusters" $ do
      sort (clusters (mkG ["a", "b"] []))
        `shouldBe` ["a" :| [], "b" :| []]
      sort (clusters (mkG ["a", "b"] [(e, "a", "b")]))
        `shouldBe` ["a" :| []]
    it "detects 2-cyclic-graph clusters" $ do
      sort (clusters (mkG ["a", "b"] [(e, "a", "b"), (e, "b", "a")]))
        -- Returns empty list on cyclic graphs
        `shouldBe` []
    it "detects 3-cyclic-graph clusters" $ do
      sort (clusters (mkG ["a", "b", "c"] [(e, "a", "b"), (e, "b", "a")]))
        -- a,b are not included, because they have no mothers.
        `shouldBe` ["c" :| []]
