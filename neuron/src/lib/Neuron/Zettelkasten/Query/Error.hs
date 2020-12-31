{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Query.Error where

import Data.Aeson (FromJSON, ToJSON)
import Neuron.Zettelkasten.Connection (Connection)
import Neuron.Zettelkasten.ID (ZettelID (..))
import Relude

-- | Error in evaluating a query
--
-- This error is only thrown when *using* (eg: in HTML) the query results.
data QueryResultError
  = QueryResultError_NoSuchZettel (Maybe Connection) ZettelID
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

showQueryResultError :: QueryResultError -> Text
showQueryResultError (QueryResultError_NoSuchZettel _conn zid) =
  "no such zettel: " <> unZettelID zid

missingZids :: NonEmpty QueryResultError -> NonEmpty ZettelID
missingZids = fmap $ \case
  QueryResultError_NoSuchZettel _ zid -> zid