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
    zettelsByTag,
  )
where

import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), Value, object)
import qualified Data.Map.Strict as Map
import Data.TagTree (Tag, TagQuery (..), matchTagQuery, matchTagQueryMulti, tagTree)
import Data.Tree (Tree (..))
import Neuron.Zettelkasten.Graph (backlinks, getZettel)
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.ID (ZettelID)
import Neuron.Zettelkasten.Query.Error (QueryResultError (..))
import Neuron.Zettelkasten.Query.Graph (GraphQuery (..))
import Neuron.Zettelkasten.Zettel
  ( Zettel,
    ZettelQuery (..),
    ZettelT (..),
    sortZettelsReverseChronological,
  )
import Neuron.Zettelkasten.Zettel.Error (ZettelIssue)
import Relude

runZettelQuery :: [Zettel] -> ZettelQuery r -> Either QueryResultError r
runZettelQuery zs = \case
  ZettelQuery_ZettelByID zid conn ->
    case find ((== zid) . zettelID) zs of
      Nothing ->
        Left $ QueryResultError_NoSuchZettel (Just conn) zid
      Just z ->
        Right z
  ZettelQuery_ZettelsByTag pats _mconn _mview ->
    Right $ zettelsByTag zs pats
  ZettelQuery_Tags pats ->
    Right $ Map.filterWithKey (const . flip matchTagQuery pats) allTags
  ZettelQuery_TagZettel _tag ->
    Right ()
  where
    allTags :: Map.Map Tag Natural
    allTags =
      Map.fromListWith (+) $
        concatMap (\Zettel {..} -> (,1) <$> toList zettelTags) zs

zettelsByTag :: [Zettel] -> TagQuery -> [Zettel]
zettelsByTag zs q =
  sortZettelsReverseChronological $
    flip filter zs $ \Zettel {..} ->
      matchTagQueryMulti (toList zettelTags) q

runGraphQuery :: ZettelGraph -> GraphQuery r -> Either QueryResultError r
runGraphQuery g = \case
  GraphQuery_Id -> Right g
  GraphQuery_BacklinksOf conn zid ->
    case getZettel zid g of
      Nothing ->
        Left $ QueryResultError_NoSuchZettel conn zid
      Just z ->
        Right $ backlinks (maybe isJust (const (== conn)) conn) z g

zettelQueryResultJson ::
  forall r.
  (ToJSON (ZettelQuery r)) =>
  ZettelQuery r ->
  Either QueryResultError r ->
  -- Zettels that cannot be parsed by neuron
  Map ZettelID ZettelIssue ->
  Value
zettelQueryResultJson q er skippedZettels =
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
        toJSON r
      ZettelQuery_ZettelsByTag _ _mconn _mview ->
        toJSON r
      ZettelQuery_Tags _ ->
        toJSON $ fmap treeToJson . tagTree $ r
      ZettelQuery_TagZettel _ ->
        toJSON r
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
  Either QueryResultError r ->
  -- Zettels that cannot be parsed by neuron (and as such are excluded from the graph)
  Map ZettelID ZettelIssue ->
  Value
graphQueryResultJson q er skippedZettels =
  toJSON $
    object
      [ "query" .= toJSON q,
        either
          (\e -> "error" .= toJSON e)
          (\r -> "result" .= resultJson r)
          er,
        "skipped" .= skippedZettels
      ]
  where
    resultJson :: r -> Value
    resultJson r = case q of
      GraphQuery_Id ->
        toJSON r
      GraphQuery_BacklinksOf _ _ ->
        toJSON r
