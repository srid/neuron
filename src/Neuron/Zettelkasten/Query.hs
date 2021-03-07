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

import Data.Aeson (KeyValue ((.=)), ToJSON (toJSON), Value, object)
import Data.Tagged
import Neuron.Zettelkasten.Graph (backlinks, getZettel)
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.ID (ZettelID)
import Neuron.Zettelkasten.Query.Graph (GraphQuery (..))
import Neuron.Zettelkasten.Zettel (MissingZettel)
import Neuron.Zettelkasten.Zettel.Error (ZettelIssue)
import Relude

runGraphQuery :: ZettelGraph -> GraphQuery r -> Either MissingZettel r
runGraphQuery g = \case
  GraphQuery_Id -> Right g
  GraphQuery_BacklinksOf conn zid ->
    case getZettel zid g of
      Nothing ->
        Left $ Tagged zid
      Just z ->
        Right $ backlinks (maybe isJust (const (== conn)) conn) z g

graphQueryResultJson ::
  forall r.
  (ToJSON (GraphQuery r)) =>
  GraphQuery r ->
  r ->
  Map ZettelID ZettelIssue ->
  Value
graphQueryResultJson q r errors =
  toJSON $
    object
      [ "query" .= toJSON q,
        "result" .= resultJson r,
        "errors" .= errors
      ]
  where
    resultJson :: r -> Value
    resultJson r' = case q of
      GraphQuery_Id ->
        toJSON r'
      GraphQuery_BacklinksOf _ _ ->
        toJSON r'
