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
import Neuron.Zettelkasten.Query.Type
import Neuron.Zettelkasten.Zettel
import Relude
import System.FilePath

-- | Run the given query on zettels list and return the results.
runQuery :: [Zettel] -> Query r -> r
runQuery zs = \case
  Query_ZettelByID zid _ ->
    find ((== zid) . zettelID) zs
  Query_ZettelsByTag pats _mconn _mview ->
    sortZettelsReverseChronological $ flip filter zs $ \Zettel {..} ->
      and $ flip fmap pats $ \pat ->
        any (tagMatch pat) zettelTags
  Query_Tags [] ->
    allTags
  Query_Tags pats ->
    Map.filterWithKey (const . tagMatchAny pats) allTags
  where
    allTags :: Map.Map Tag Natural
    allTags =
      Map.fromListWith (+) $
        concatMap (\Zettel {..} -> (,1) <$> zettelTags) zs

runQueryGraph :: ZettelGraph -> QueryGraph r -> r
runQueryGraph g = \case
  QueryGraph_Id -> g

queryResultJson ::
  forall r.
  (ToJSON (Query r)) =>
  FilePath ->
  Query r ->
  r ->
  -- Zettels that cannot be parsed by neuron
  Map ZettelID Text ->
  Value
queryResultJson notesDir q r errors =
  toJSON $
    object
      [ "query" .= toJSON q,
        "result" .= resultJson,
        "errors" .= errors
      ]
  where
    resultJson :: Value
    resultJson = case q of
      Query_ZettelByID _ _mconn ->
        toJSON $ zettelJsonFull <$> r
      Query_ZettelsByTag _ _mconn _mview ->
        toJSON $ zettelJsonFull <$> r
      Query_Tags _ ->
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

queryGraphResultJson ::
  forall r.
  (ToJSON (QueryGraph r)) =>
  QueryGraph r ->
  r ->
  -- Zettels that cannot be parsed by neuron (and as such are excluded from the graph)
  Map ZettelID Text ->
  Value
queryGraphResultJson q r errors =
  toJSON $
    object
      [ "query" .= toJSON q,
        "result" .= resultJson,
        "errors" .= errors
      ]
  where
    resultJson :: Value
    resultJson = case q of
      QueryGraph_Id ->
        toJSON r
