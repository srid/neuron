{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Query.Error where

import Neuron.Zettelkasten.ID (InvalidID)
import Neuron.Zettelkasten.Query.Theme
import Relude
import Text.URI

data QueryError
  = QueryError_InvalidQuery URI InvalidQuery
  deriving (Eq, Show)

data InvalidQuery
  = InvalidQuery_InvalidID InvalidID
  | InvalidQuery_Unsupported
  | InvalidQuery_UnsupportedHost
  | InvalidQuery_BadView InvalidLinkView
  deriving (Eq, Show)

queryErrorUri :: QueryError -> URI
queryErrorUri = \case
  QueryError_InvalidQuery uri _ -> uri
