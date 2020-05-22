{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Query.Eval
  ( expandQueries,
    evalQueryLink,
  )
where

import Control.Monad.Except
import Control.Monad.Writer
import Data.Dependent.Sum
import Data.Some
import Neuron.Markdown
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.Query
import Neuron.Zettelkasten.Query.Error
import Neuron.Zettelkasten.Query.Parser (queryFromMarkdownLink, queryFromURILink)
import Neuron.Zettelkasten.Zettel
import Reflex.Dom.Pandoc (URILink)
import Relude
import qualified Text.Pandoc.Walk as W

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

-- | Expand query links in the Pandoc document.
--
-- * Report any errors via MonadError
-- * Write connections detected in MonadWriter
-- * Do a two-stage transform, to handle block links and inline links separately.
expandQueries ::
  forall m.
  ( MonadError QueryError m,
    -- Running queries requires the zettels list.
    MonadReader [Zettel] m,
    -- Report back connections formed by (running) the queries
    MonadWriter [(Maybe Connection, Zettel)] m
  ) =>
  Zettel ->
  m Zettel
expandQueries z@Zettel {..} = do
  void $ flip W.walkM zettelContent $ \inline -> do
    case pandocLinkInline inline of
      Just ml -> do
        void $ expandAST ml
      _ ->
        pure ()
    pure inline
  pure z
  where
    -- Replace the link node with the query result AST node.
    --
    -- Depending on the link time, we replace with an inline or a block.
    expandAST :: MarkdownLink -> m ()
    expandAST ml = do
      mq <- liftEither $ runExcept $ withExceptT Left (queryFromMarkdownLink ml)
      case mq of
        Nothing -> pure ()
        Just someQ -> do
          qres <- withSome someQ $ \q -> do
            zs <- ask
            -- run query using data from MonadReader
            pure $ q :=> Identity (runQuery zs q)
          -- tell connections using MonadWriter
          tell $ getConnections qres
    getConnections :: DSum Query Identity -> [(Maybe Connection, Zettel)]
    getConnections = \case
      Query_ZettelByID _ mconn :=> Identity mres ->
        maybe [] pure $ (mconn,) <$> mres
      Query_ZettelsByTag _ mconn _mview :=> Identity res ->
        (mconn,) <$> res
      Query_Tags _ :=> _ ->
        mempty
