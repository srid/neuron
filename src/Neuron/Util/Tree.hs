{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Util.Tree
  ( mkTreeFromPaths,
    annotatePathsWith,
    foldTreeOnWith,
  )
where

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

annotatePathsWith :: ([a] -> ann) -> Tree a -> Tree (a, ann)
annotatePathsWith f = go []
  where
    go root (Node rel children) =
      let path = rel : root
       in Node (rel, f $ reverse path) $ fmap (go path) children

-- | Fold one-child nodes that satisfy a predicate
--
-- The given function is called to fold a parent and its (only) child.
foldTreeOnWith :: (a -> Bool) -> (a -> a -> a) -> Tree a -> Tree a
foldTreeOnWith p f = go
  where
    go (Node parent children) =
      case fmap go children of
        [Node child grandChildren]
          | p parent -> Node (f parent child) grandChildren
        xs -> Node parent xs
