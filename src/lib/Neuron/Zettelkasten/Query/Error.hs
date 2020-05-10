{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Query.Error where

import Neuron.Zettelkasten.ID (InvalidID, ZettelID)
import Relude
import Text.URI

type QueryError = Either QueryParseError QueryResultError

data QueryParseError
  = QueryParseError_InvalidID URI InvalidID
  | QueryParseError_UnsupportedHost URI
  | QueryParseError_BadLocation URI
  deriving (Eq, Show)

-- | This error is only thrown when *using* (eg: in HTML) the query results.
data QueryResultError = QueryResultError_NoSuchZettel ZettelID
  deriving (Eq, Show)

queryParseErrorUri :: QueryParseError -> URI
queryParseErrorUri = \case
  QueryParseError_InvalidID uri _ -> uri
  QueryParseError_UnsupportedHost uri -> uri
  QueryParseError_BadLocation uri -> uri
