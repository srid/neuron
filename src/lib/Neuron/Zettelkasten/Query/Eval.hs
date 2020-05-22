{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Query.Eval where

import Control.Monad.Except
import Control.Monad.Writer
import Data.Dependent.Sum
import Data.Some
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.Query
import Neuron.Zettelkasten.Query.Error
import Neuron.Zettelkasten.Query.Parser (queryFromURILink)
import Neuron.Zettelkasten.Zettel
import Reflex.Dom.Pandoc.URILink (URILink, queryURILinks)
import Relude
import Text.Pandoc.Definition (Pandoc)

-- | Evaluate the given query link and return its results.
--
-- Return Nothing if the link is not a query.
--
-- We need the full list of zettels, for running the query against.
evalQueryLink ::
  ( MonadError QueryParseError m,
    MonadReader [Zettel] m
  ) =>
  URILink ->
  m (Maybe (DSum Query Identity))
evalQueryLink link = do
  mq <- queryFromURILink link
  case mq of
    Nothing -> pure Nothing
    Just someQ -> fmap Just $ do
      withSome someQ $ \q -> do
        zs <- ask
        pure $ q :=> Identity (runQuery zs q)

queryConnections ::
  forall m.
  ( MonadError QueryParseError m,
    -- Running queries requires the zettels list.
    MonadReader [Zettel] m
  ) =>
  Pandoc ->
  m [(Maybe Connection, Zettel)]
queryConnections doc = do
  let uriLinks = queryURILinks doc
  fmap concat $ forM uriLinks $ \ul ->
    evalQueryLink ul >>= \case
      Nothing -> pure []
      Just res -> do
        let cs = getConnections res
        -- tell cs
        pure cs
  where
    getConnections :: DSum Query Identity -> [(Maybe Connection, Zettel)]
    getConnections = \case
      Query_ZettelByID _ mconn :=> Identity mres ->
        maybe [] pure $ (mconn,) <$> mres
      Query_ZettelsByTag _ mconn _mview :=> Identity res ->
        (mconn,) <$> res
      Query_Tags _ :=> _ ->
        mempty
