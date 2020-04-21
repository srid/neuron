{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Util.Tree
  ( mkTreeFromPaths,
    annotatePathsWith,
    foldSingleParentsWith,
  )
where

import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import Data.Tree
import Relude
import Relude.Extra.Group

mkTreeFromPaths :: Ord a => [[a]] -> Forest a
mkTreeFromPaths paths = uncurry mkNode <$> Map.assocs groups
  where
    groups = fmap tail <$> groupBy head (mapMaybe nonEmpty paths)
    mkNode label children =
      Node label $ mkTreeFromPaths $ toList children

annotatePathsWith :: (NonEmpty a -> ann) -> Tree a -> Tree (a, ann)
annotatePathsWith f = go []
  where
    go ancestors (Node rel children) =
      let path = rel :| ancestors
       in Node (rel, f $ NE.reverse path) $ fmap (go $ toList path) children

-- | Fold nodes with one child using the given function
--
-- The function is called with the parent and the only child. If a Just value is
-- returned, folding happens with that value, otherwise there is no effect.
foldSingleParentsWith :: (a -> a -> Maybe a) -> Tree a -> Tree a
foldSingleParentsWith f = go
  where
    go (Node parent children) =
      case fmap go children of
        [Node child grandChildren]
          | Just new <- f parent child -> Node new grandChildren
        xs -> Node parent xs
