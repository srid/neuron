{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Query.Eval
  ( runQuery,
    queryConnections,
  )
where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.Writer (MonadWriter (tell))
import Data.Dependent.Sum (DSum (..))
import Data.Some (Some, withSome)
import Neuron.Zettelkasten.Connection (Connection)
import Neuron.Zettelkasten.Query (runZettelQuery)
import Neuron.Zettelkasten.Query.Error (QueryResultError)
import Neuron.Zettelkasten.Zettel
  ( Zettel,
    ZettelQuery (..),
    ZettelT (..),
  )
import Relude
import Text.Pandoc.Definition (Block)

runQuery :: [Zettel] -> Some ZettelQuery -> Either QueryResultError (DSum ZettelQuery Identity)
runQuery zs =
  flip runReaderT zs . runSomeZettelQuery

-- Query connections in the given zettel
--
-- Tell all errors; query parse errors (as already stored in `Zettel`) as well
-- query result errors.
queryConnections ::
  ( -- Errors are written aside, accumulating valid connections.
    MonadWriter [QueryResultError] m,
    -- Running queries requires the zettels list.
    MonadReader [Zettel] m
  ) =>
  Zettel ->
  m [((Connection, [Block]), Zettel)]
queryConnections Zettel {..} = do
  fmap concat $
    forM zettelQueries $ \(someQ, ctx) ->
      runExceptT (runSomeZettelQuery someQ) >>= \case
        Left e -> do
          tell [e]
          pure mempty
        Right res ->
          pure $ first (,ctx) <$> getConnections res
  where
    getConnections :: DSum ZettelQuery Identity -> [(Connection, Zettel)]
    getConnections = \case
      ZettelQuery_ZettelByID _ conn :=> Identity res ->
        [(conn, res)]
      ZettelQuery_ZettelsByTag _ conn _mview :=> Identity res ->
        (conn,) <$> res
      ZettelQuery_Tags _ :=> _ ->
        mempty
      ZettelQuery_TagZettel _ :=> _ ->
        mempty

runSomeZettelQuery ::
  ( MonadError QueryResultError m,
    MonadReader [Zettel] m
  ) =>
  Some ZettelQuery ->
  m (DSum ZettelQuery Identity)
runSomeZettelQuery someQ =
  withSome someQ $ \q -> do
    zs <- ask
    case runZettelQuery zs q of
      Left e ->
        throwError e
      Right res ->
        pure $ q :=> Identity res
