{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Query.Eval
  ( evalZettelLinks,
    expandQueries,
  )
where

import Control.Monad.Except
import Control.Monad.Writer
import Data.Dependent.Sum
import qualified Data.Map.Strict as Map
import Data.Some
import Data.Traversable (for)
import Lucid
import Neuron.Markdown
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.Query
import Neuron.Zettelkasten.Query.Error
import Neuron.Zettelkasten.Query.Parser (queryFromMarkdownLink)
import Neuron.Zettelkasten.Query.View (buildQueryView, renderQueryLink)
import Neuron.Zettelkasten.Zettel
import Relude
import qualified Text.Pandoc.Builder as B
import qualified Text.Pandoc.Walk as W
import qualified Text.URI as URI

-- TODO: Use MonadReader and MonadWriter
expandQueries ::
  (MonadError QueryError m, MonadReader [Zettel] m, MonadWriter [(Connection, Zettel)] m) =>
  Zettel ->
  m Zettel
expandQueries z@Zettel {..} = do
  newPandoc <- flip W.walkM zettelContent $ \case
    x@(B.Link _attr [B.Str linkText] (url, _title)) -> do
      case URI.mkURI url of
        Nothing -> pure x
        Just uri -> do
          let ml = MarkdownLink linkText uri
          mq <- liftEither $ runExcept $ withExceptT Left (queryFromMarkdownLink ml)
          case mq of
            Nothing -> pure x
            Just someQ -> do
              qres <- withSome someQ $ \q -> do
                zs <- ask
                -- run query using data from MonadReader
                let res = q :=> Identity (runQuery zs q)
                pure res
              -- tell connections using MonadWriter
              let conns = getConnections qres
              tell conns
              -- create Inline for ml here.
              -- TODO delineate and render
              liftEither $ runExcept $ withExcept Right $ buildQueryView qres
    x -> pure x
  pure $ z {zettelContent = newPandoc}
  where
    getConnections :: DSum Query Identity -> [(Connection, Zettel)]
    getConnections = \case
      Query_ZettelByID _ mconn :=> Identity mres ->
        maybe [] pure $ (fromMaybe Folgezettel mconn,) <$> mres
      Query_ZettelsByTag _ mconn _mview :=> Identity res ->
        (fromMaybe Folgezettel mconn,) <$> res
      _ ->
        []

type EvalResult = ([(Connection, Zettel)], Html ())

-- | Evaluate all queries in a zettel
--
-- Return the queries with results as a map from the original markdown link.
evalZettelLinks ::
  MonadError QueryError m =>
  [Zettel] ->
  Zettel ->
  m (Map MarkdownLink EvalResult)
evalZettelLinks zettels Zettel {..} =
  fmap (Map.fromList . catMaybes) $ for (extractAutoLinks zettelContent) $ \ml ->
    fmap (ml,) <$> evalMarkdownLink zettels ml

-- | Fully evaluate the query in a markdown link.
evalMarkdownLink ::
  MonadError QueryError m =>
  [Zettel] ->
  MarkdownLink ->
  m (Maybe EvalResult)
evalMarkdownLink zettels ml =
  liftEither $ runExcept $ do
    mq <- withExcept Left $ queryFromMarkdownLink ml
    case mq of
      Nothing -> pure Nothing
      Just someQ -> Just <$> do
        let qres = withSome someQ $ \q ->
              q :=> Identity (runQuery zettels q)
            conns = getConnections qres
        view <-
          withExcept Right $
            renderQueryLink qres
        pure (conns, view)
  where
    getConnections :: DSum Query Identity -> [(Connection, Zettel)]
    getConnections = \case
      Query_ZettelByID _ mconn :=> Identity mres ->
        maybe [] pure $ (fromMaybe Folgezettel mconn,) <$> mres
      Query_ZettelsByTag _ mconn _mview :=> Identity res ->
        (fromMaybe Folgezettel mconn,) <$> res
      _ ->
        []
