{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Query.Error where

import Neuron.Zettelkasten.ID (InvalidID, ZettelID)
import Neuron.Zettelkasten.Query.Theme
import Relude
import Text.URI

data QueryParseError
  = QueryParseError_InvalidID URI InvalidID
  | QueryParseError_Unsupported URI
  | QueryParseError_UnsupportedHost URI
  | QueryParseError_BadView URI InvalidLinkView
  deriving (Eq, Show)

data QueryResultError = QueryResultError_NoSuchZettel ZettelID
  deriving (Eq, Show)

queryParseErrorUri :: QueryParseError -> URI
queryParseErrorUri = \case
  QueryParseError_InvalidID uri _ -> uri
  QueryParseError_Unsupported uri -> uri
  QueryParseError_UnsupportedHost uri -> uri
  QueryParseError_BadView uri _ -> uri
