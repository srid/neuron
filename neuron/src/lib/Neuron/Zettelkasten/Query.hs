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
module Neuron.Zettelkasten.Query
  ( runZettelQuery,
    runGraphQuery,
    zettelQueryResultJson,
    graphQueryResultJson,
  )
where

import Control.Monad.Except
import Data.Aeson
import qualified Data.Map.Strict as Map
import Data.TagTree (Tag, tagMatch, tagMatchAny, tagTree)
import Data.Tree (Tree (..))
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.Graph (backlinks, getConnections, getZettel)
import Neuron.Zettelkasten.Graph.Type
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Query.Error (QueryResultError (..))
import Neuron.Zettelkasten.Query.Graph
import Neuron.Zettelkasten.Zettel
import Relude
import System.FilePath

runZettelQuery :: [Zettel] -> ZettelQuery r -> Either QueryResultError r
runZettelQuery zs = \case
  ZettelQuery_ZettelByID zid _ ->
    case find ((== zid) . zettelID) zs of
      Nothing ->
        Left $ QueryResultError_NoSuchZettel zid
      Just z ->
        Right z
  ZettelQuery_ZettelsByTag pats _mconn _mview ->
    Right $ sortZettelsReverseChronological $ flip filter zs $ \Zettel {..} ->
      and $ flip fmap pats $ \pat ->
        any (tagMatch pat) zettelTags
  ZettelQuery_Tags [] ->
    Right allTags
  ZettelQuery_Tags pats ->
    Right $ Map.filterWithKey (const . tagMatchAny pats) allTags
  where
    allTags :: Map.Map Tag Natural
    allTags =
      Map.fromListWith (+) $
        concatMap (\Zettel {..} -> (,1) <$> zettelTags) zs

runGraphQuery :: ZettelGraph -> GraphQuery r -> Either QueryResultError r
runGraphQuery g = \case
  GraphQuery_Id -> Right g
  GraphQuery_BacklinksOf conn zid ->
    case getZettel zid g of
      Nothing ->
        Left $ QueryResultError_NoSuchZettel zid
      Just z ->
        Right $ backlinks (maybe isJust (const (== conn)) conn) z g

zettelJsonFull :: forall a. KeyValue a => FilePath -> Zettel -> [a]
zettelJsonFull notesDir z@Zettel {..} =
  [ "path" .= (notesDir </> zettelIDSourceFileName zettelID)
  ]
    <> zettelJson z

zettelQueryResultJson ::
  forall r.
  (ToJSON (ZettelQuery r)) =>
  FilePath ->
  ZettelQuery r ->
  Either QueryResultError r ->
  -- Zettels that cannot be parsed by neuron
  Map ZettelID ZettelError ->
  Value
zettelQueryResultJson notesDir q er skippedZettels =
  toJSON $
    object
      [ "query" .= toJSON q,
        either
          (\e -> "error" .= toJSON e)
          (\r -> "result" .= toJSON (resultJson r))
          er,
        "skipped" .= skippedZettels
      ]
  where
    resultJson :: r -> Value
    resultJson r = case q of
      ZettelQuery_ZettelByID _ _mconn ->
        object $ zettelJsonFull notesDir r
      ZettelQuery_ZettelsByTag _ _mconn _mview ->
        toJSON $ fmap (object . zettelJsonFull notesDir) r
      ZettelQuery_Tags _ ->
        toJSON $ fmap treeToJson . tagTree $ r
    treeToJson (Node (tag, count) children) =
      object
        [ "name" .= tag,
          "count" .= count,
          "children" .= fmap treeToJson children
        ]

graphQueryResultJson ::
  forall r.
  (ToJSON (GraphQuery r)) =>
  FilePath ->
  GraphQuery r ->
  Either QueryResultError r ->
  -- Zettels that cannot be parsed by neuron (and as such are excluded from the graph)
  Map ZettelID ZettelError ->
  Value
graphQueryResultJson notesDir q er skippedZettels =
  toJSON $
    object
      [ "query" .= toJSON q,
        either
          (\e -> "error" .= toJSON e)
          (\r -> "result" .= toJSON (resultJson r))
          er,
        "skipped" .= skippedZettels
      ]
  where
    edgeJson :: Connection -> Zettel -> Value
    edgeJson connection zettel =
      object $ ["connection" .= toJSON connection] <> zettelJsonFull notesDir zettel
    resultJson :: r -> Value
    resultJson r = case q of
      GraphQuery_Id ->
        toJSON (getConnections r)
      GraphQuery_BacklinksOf _ _ ->
        toJSON $ fmap (uncurry edgeJson) r
