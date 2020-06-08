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
import Neuron.Markdown (ZettelParseError)
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

runGraphQuery :: ZettelGraph -> GraphQuery r -> r
runGraphQuery g = \case
  GraphQuery_Id -> g

zettelQueryResultJson ::
  forall r.
  (ToJSON (ZettelQuery r)) =>
  FilePath ->
  ZettelQuery r ->
  Either QueryResultError r ->
  -- Zettels that cannot be parsed by neuron
  Map ZettelID ZettelParseError ->
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
        zettelJsonFull r
      ZettelQuery_ZettelsByTag _ _mconn _mview ->
        toJSON $ fmap zettelJsonFull r
      ZettelQuery_Tags _ ->
        toJSON $ fmap treeToJson . tagTree $ r
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
  Map ZettelID ZettelParseError ->
  Value
graphQueryResultJson q r skippedZettels =
  toJSON $
    object
      [ "query" .= toJSON q,
        "result" .= resultJson,
        "skipped" .= skippedZettels
      ]
  where
    resultJson :: Value
    resultJson = case q of
      GraphQuery_Id ->
        toJSON r
