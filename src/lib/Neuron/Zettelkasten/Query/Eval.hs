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

-- | Expand query links in the Pandoc document.
--
-- * Report any errors via MonadError
-- * Write connections detected in MonadWriter
-- * Do a two-stage transform, to handle block links and inline links separately.
expandQueries ::
  forall m.
  (MonadError QueryError m, MonadReader [Zettel] m, MonadWriter [(Maybe Connection, Zettel)] m) =>
  Zettel ->
  m Zettel
expandQueries z@Zettel {..} = do
  -- Transform block links (paragraph with one link)
  -- Only block links can contain multi-zettel queries as they produce Block (not Inline) view.
  ast1 <- flip W.walkM zettelContent $ \blk ->
    case pandocLinkBlock blk of
      Just ml -> do
        expandAST ml >>= \case
          Just (Right newBlk) -> pure newBlk
          _ -> pure blk
      _ -> pure blk
  -- Transform the rest (by scanning all inline links)
  ast2 <- flip W.walkM ast1 $ \inline ->
    case pandocLinkInline inline of
      Just ml -> do
        expandAST ml >>= \case
          Just (Left newInline) -> pure newInline
          _ -> pure inline
      _ -> pure inline
  pure $ z {zettelContent = ast2}
  where
    -- Replace the link node with the query result AST node.
    --
    -- Depending on the link time, we replace with an inline or a block.
    expandAST :: MarkdownLink -> m (Maybe (Either B.Inline B.Block))
    expandAST ml = do
      mq <- liftEither $ runExcept $ withExceptT Left (queryFromMarkdownLink ml)
      case mq of
        Nothing -> pure Nothing
        Just someQ -> fmap Just $ do
          qres <- withSome someQ $ \q -> do
            zs <- ask
            -- run query using data from MonadReader
            pure $ q :=> Identity (runQuery zs q)
          -- tell connections using MonadWriter
          tell $ getConnections qres
          -- create Inline for ml here.
          liftEither $ runExcept $ do
            withExcept Right (buildQueryView qres)
    getConnections :: DSum Query Identity -> [(Maybe Connection, Zettel)]
    getConnections = \case
      Query_ZettelByID _ mconn :=> Identity mres ->
        maybe [] pure $ (mconn,) <$> mres
      Query_ZettelsByTag _ mconn _mview :=> Identity res ->
        (mconn,) <$> res
      Query_Tags _ :=> _ ->
        mempty
