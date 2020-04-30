{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Query.Error where

import Neuron.Zettelkasten.ID (InvalidID, ZettelID)
import Neuron.Zettelkasten.Query.Theme
import Relude
import Text.URI

data QueryError
  = QueryError_InvalidQuery URI InvalidQuery
  | QueryError_InvalidQueryView URI InvalidLinkView
  | QueryError_ZettelNotFound URI ZettelID
  deriving (Eq, Show)

data InvalidQuery
  = InvalidQuery_InvalidID InvalidID
  | InvalidQuery_Unsupported
  | InvalidQuery_UnsupportedHost
  deriving (Eq, Show)

queryErrorUri :: QueryError -> URI
queryErrorUri = \case
  QueryError_InvalidQuery uri _ -> uri
  QueryError_InvalidQueryView uri _ -> uri
  QueryError_ZettelNotFound uri _ -> uri
