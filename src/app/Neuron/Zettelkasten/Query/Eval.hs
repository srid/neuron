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
import Neuron.Zettelkasten.Query.Parser (queryFromMarkdownLink)
import Neuron.Zettelkasten.Query.View (buildQueryView)
import Neuron.Zettelkasten.Zettel
import Relude
import qualified Text.Pandoc.Builder as B
import qualified Text.Pandoc.Walk as W
import qualified Text.URI as URI

-- TODO: Use MonadReader and MonadWriter
expandQueries ::
  forall m.
  (MonadError QueryError m, MonadReader [Zettel] m, MonadWriter [(Connection, Zettel)] m) =>
  Zettel ->
  m Zettel
expandQueries z@Zettel {..} = do
  -- Transform block links (paragraph with one link)
  -- Only block links can contain multi-zettel queries as they produce Block (not Inline) view.
  ast1 <- flip W.walkM zettelContent $ \case
    x@(B.Para [B.Link _attr [B.Str linkText] (url, _title)]) ->
      expandAST linkText url >>= \case
        Just (_uri, Right block) ->
          pure block
        _ -> pure x
    x -> pure x
  -- Transform the rest (by scanning all inline)
  ast2 <- flip W.walkM ast1 $ \case
    x@(B.Link _attr [B.Str linkText] (url, _title)) -> do
      expandAST linkText url >>= \case
        Nothing -> pure x
        Just (_uri, Left inline) -> pure inline
        Just (uri, Right _block) ->
          throwError $ Left $ QueryParseError_BadLocation uri
    x -> pure x
  pure $ z {zettelContent = ast2}
  where
    expandAST :: Text -> Text -> m (Maybe (URI.URI, Either B.Inline B.Block))
    expandAST linkText url = do
      case URI.mkURI url of
        Nothing -> pure Nothing
        Just uri -> do
          let ml = MarkdownLink linkText uri
          mq <- liftEither $ runExcept $ withExceptT Left (queryFromMarkdownLink ml)
          case mq of
            Nothing -> pure Nothing
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
              liftEither $ runExcept $ do
                Just . (uri,) <$> withExcept Right (buildQueryView qres)
    -- FIXME: This is ugly; need to handle at type level.
    -- Right _ -> throwError $ Left $ QueryParseError_BadLocation uri
    getConnections :: DSum Query Identity -> [(Connection, Zettel)]
    getConnections = \case
      Query_ZettelByID _ mconn :=> Identity mres ->
        maybe [] pure $ (fromMaybe Folgezettel mconn,) <$> mres
      Query_ZettelsByTag _ mconn _mview :=> Identity res ->
        (fromMaybe Folgezettel mconn,) <$> res
      _ ->
        []
