{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Query.Error where

import Data.Aeson (FromJSON, ToJSON)
import Neuron.Orphans ()
import Neuron.Zettelkasten.ID (InvalidID, ZettelID (..))
import Relude
import Text.URI (URI)
import qualified Text.URI as URI

type QueryError = Either QueryParseError QueryResultError

data QueryParseError
  = QueryParseError_InvalidID URI InvalidID
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Error in evaluating a query
--
-- This error is only thrown when *using* (eg: in HTML) the query results.
data QueryResultError
  = QueryResultError_NoSuchZettel ZettelID
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

queryParseErrorUri :: QueryParseError -> URI
queryParseErrorUri = \case
  QueryParseError_InvalidID uri _ -> uri

showQueryError :: QueryError -> Text
showQueryError = \case
  Left qe ->
    showQueryParseError qe
  Right re ->
    showQueryResultError re

showQueryParseError :: QueryParseError -> Text
showQueryParseError qe =
  let uri = URI.render (queryParseErrorUri qe)
   in uri <> ": " <> case qe of
        QueryParseError_InvalidID _uri e'' ->
          "invalidID: " <> show e''

showQueryResultError :: QueryResultError -> Text
showQueryResultError (QueryResultError_NoSuchZettel zid) =
  "no such zettel: " <> zettelIDRaw zid
