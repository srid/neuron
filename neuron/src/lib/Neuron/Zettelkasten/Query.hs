{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Queries to the Zettel store
module Neuron.Zettelkasten.Query where

import Control.Monad.Except
import Data.Aeson
import qualified Data.Map.Strict as Map
import Data.TagTree (Tag, tagMatch, tagMatchAny, tagTree)
import Data.Tree (Tree (..))
import Neuron.Zettelkasten.Graph.Type
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Query.Graph
import Neuron.Zettelkasten.Zettel
import Relude
import System.FilePath

runZettelQuery :: [Zettel] -> ZettelQuery r -> r
runZettelQuery zs = \case
  ZettelQuery_ZettelByID zid _ ->
    find ((== zid) . zettelID) zs
  ZettelQuery_ZettelsByTag pats _mconn _mview ->
    sortZettelsReverseChronological $ flip filter zs $ \Zettel {..} ->
      and $ flip fmap pats $ \pat ->
        any (tagMatch pat) zettelTags
  ZettelQuery_Tags [] ->
    allTags
  ZettelQuery_Tags pats ->
    Map.filterWithKey (const . tagMatchAny pats) allTags
  where
    allTags :: Map.Map Tag Natural
    allTags =
      Map.fromListWith (+) $
        concatMap (\Zettel {..} -> (,1) <$> zettelTags) zs

runGraphQuery :: ZettelGraph -> GraphQuery r -> r
runGraphQuery g = \case
  GraphQuery_Id -> g

zettelQueryResultJson ::
  forall r.
  (ToJSON (ZettelQuery r)) =>
  FilePath ->
  ZettelQuery r ->
  r ->
  -- Zettels that cannot be parsed by neuron
  Map ZettelID Text ->
  Value
zettelQueryResultJson notesDir q r errors =
  toJSON $
    object
      [ "query" .= toJSON q,
        "result" .= resultJson,
        "errors" .= errors
      ]
  where
    resultJson :: Value
    resultJson = case q of
      ZettelQuery_ZettelByID _ _mconn ->
        toJSON $ zettelJsonFull <$> r
      ZettelQuery_ZettelsByTag _ _mconn _mview ->
        toJSON $ zettelJsonFull <$> r
      ZettelQuery_Tags _ ->
        toJSON $ treeToJson <$> tagTree r
    zettelJsonFull :: Zettel -> Value
    zettelJsonFull z@Zettel {..} =
      object $
        [ "path" .= (notesDir </> zettelIDSourceFileName zettelID)
        ]
          <> zettelJson z
    treeToJson (Node (tag, count) children) =
      object
        [ "name" .= tag,
          "count" .= count,
          "children" .= fmap treeToJson children
        ]

graphQueryResultJson ::
  forall r.
  (ToJSON (GraphQuery r)) =>
  GraphQuery r ->
  r ->
  -- Zettels that cannot be parsed by neuron (and as such are excluded from the graph)
  Map ZettelID Text ->
  Value
graphQueryResultJson q r errors =
  toJSON $
    object
      [ "query" .= toJSON q,
        "result" .= resultJson,
        "errors" .= errors
      ]
  where
    resultJson :: Value
    resultJson = case q of
      GraphQuery_Id ->
        toJSON r
