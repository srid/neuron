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
  ( MonadError QueryError m,
    MonadReader [Zettel] m
  ) =>
  URILink ->
  m (Maybe (DSum ZettelQuery Identity))
runQueryURILink ul = do
  mq <- liftEither $ first Left $ queryFromURILink ul
  flip traverse mq $ \q ->
    either (throwError . Right) pure =<< runExceptT (runSomeZettelQuery q)

-- Query connections in the given zettel
--
-- Tell all errors; query parse errors (as already stored in `Zettel`) as well
-- query result errors.
queryConnections ::
  ( -- Errors are written aside, accumulating valid connections.
    MonadWriter [QueryError] m,
    -- Running queries requires the zettels list.
    MonadReader [Zettel] m
  ) =>
  Zettel ->
  m [(Maybe Connection, Zettel)]
queryConnections Zettel {..} = do
  -- Report any query parse errors
  case zettelError of
    Right queryParseErrors ->
      tell $ Left <$> queryParseErrors
    Left _ ->
      pure ()
  fmap concat $
    forM zettelQueries $ \someQ ->
      runExceptT (runSomeZettelQuery someQ) >>= \case
        Left e -> do
          tell [Right e]
          pure mempty
        Right res ->
          pure $ getConnections res
  where
    getConnections :: DSum ZettelQuery Identity -> [(Maybe Connection, Zettel)]
    getConnections = \case
      ZettelQuery_ZettelByID _ mconn :=> Identity res ->
        [(mconn, res)]
      ZettelQuery_ZettelsByTag _ mconn _mview :=> Identity res ->
        (mconn,) <$> res
      ZettelQuery_Tags _ :=> _ ->
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
