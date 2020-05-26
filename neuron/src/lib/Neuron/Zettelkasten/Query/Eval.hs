{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Query.Eval
  ( runQueryURILink,
    queryConnections,
  )
where

import Control.Monad.Except
import Control.Monad.Writer
import Data.Dependent.Sum
import Data.Some
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.Query (runZettelQuery)
import Neuron.Zettelkasten.Query.Error
import Neuron.Zettelkasten.Query.Parser (queryFromURILink)
import Neuron.Zettelkasten.Zettel
import Reflex.Dom.Pandoc.URILink (URILink)
import Relude

-- | Evaluate the given query link and return its results.
--
-- Return Nothing if the link is not a query.
--
-- We need the full list of zettels, for running the query against.
runQueryURILink ::
  ( MonadError QueryParseError m,
    MonadReader [Zettel] m
  ) =>
  URILink ->
  m (Maybe (DSum ZettelQuery Identity))
runQueryURILink =
  traverse runSomeZettelQuery <=< queryFromURILink

queryConnections ::
  ( -- Errors are written aside, accumulating valid connections.
    MonadWriter [QueryParseError] m,
    -- Running queries requires the zettels list.
    MonadReader [Zettel] m
  ) =>
  Zettel ->
  m [(Maybe Connection, Zettel)]
queryConnections Zettel {..} = do
  let (queries, errors) = zettelQueries
  tell errors
  fmap concat $ forM queries $ \someQ ->
    getConnections <$> runSomeZettelQuery someQ
  where
    getConnections :: DSum ZettelQuery Identity -> [(Maybe Connection, Zettel)]
    getConnections = \case
      ZettelQuery_ZettelByID _ mconn :=> Identity mres ->
        maybe [] pure $ (mconn,) <$> mres
      ZettelQuery_ZettelsByTag _ mconn _mview :=> Identity res ->
        (mconn,) <$> res
      ZettelQuery_Tags _ :=> _ ->
        mempty

runSomeZettelQuery ::
  MonadReader [Zettel] m =>
  Some ZettelQuery ->
  m (DSum ZettelQuery Identity)
runSomeZettelQuery someQ =
  withSome someQ $ \q -> do
    zs <- ask
    let res = runZettelQuery zs q
    pure $ q :=> Identity res
