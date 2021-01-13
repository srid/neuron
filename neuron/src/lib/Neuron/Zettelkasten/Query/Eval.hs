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
    -- TODO: Move to new module?
    QueryUrlCache,
    buildQueryUrlCache,
  )
where

import Control.Monad.Writer
import Data.Dependent.Sum (DSum (..))
import qualified Data.Map.Strict as Map
import Data.Some (Some, withSome)
import Data.Tagged
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.Query (runZettelQuery)
import Neuron.Zettelkasten.Query.Parser (parseQueryLink)
import Neuron.Zettelkasten.Zettel
  ( MissingZettel,
    Zettel,
    ZettelQuery (..),
    ZettelT (..),
  )
import qualified Neuron.Zettelkasten.Zettel as Z
import Relude

runQuery :: [Zettel] -> Some ZettelQuery -> DSum ZettelQuery Identity
runQuery zs someQ =
  flip runReader zs $ runSomeZettelQuery someQ

-- Query connections in the given zettel
--
-- Tell all errors; query parse errors (as already stored in `Zettel`) as well
-- query result errors.
queryConnections ::
  forall m.
  ( -- Running queries requires the zettels list.
    MonadReader [Zettel] m,
    -- Track missing zettel links in writer
    MonadWriter [MissingZettel] m
  ) =>
  Zettel ->
  m [(ContextualConnection, Zettel)]
queryConnections Zettel {..} = do
  zs :: [Zettel] <- ask
  r1 <- fmap concat $
    forM (fst zettelQueries) $ \((zid, conn), blk) -> do
      case find ((== zid) . Z.zettelID) zs of
        Nothing -> do
          tell [Tagged zid]
          pure []
        Just z ->
          pure [((conn, blk), z)]
  r2 <- fmap concat $
    forM (snd zettelQueries) $ \someQ -> do
      qRes <- runSomeZettelQuery someQ
      links <- getConnections qRes
      pure $ first (,mempty) <$> links
  pure $ r1 <> r2
  where
    getConnections :: DSum ZettelQuery Identity -> m [(Connection, Zettel)]
    getConnections = \case
      {-
      ZettelQuery_ZettelByID _ conn :=> Identity res ->
        case res of
          Left zid -> do
            tell [zid]
            pure []
          Right z ->
            pure [(conn, z)] -}
      ZettelQuery_ZettelsByTag _ conn _mview :=> Identity res ->
        pure $ (conn,) <$> res
      ZettelQuery_Tags _ :=> _ ->
        pure mempty
      ZettelQuery_TagZettel _ :=> _ ->
        pure mempty

runSomeZettelQuery ::
  ( MonadReader [Zettel] m
  ) =>
  Some ZettelQuery ->
  m (DSum ZettelQuery Identity)
runSomeZettelQuery someQ =
  withSome someQ $ \q -> do
    zs <- ask
    let res = runZettelQuery zs q
    pure $ q :=> Identity res

type QueryUrlCache = Map Text (Either (Either MissingZettel (Connection, Zettel)) (DSum ZettelQuery Identity))

buildQueryUrlCache :: [Zettel] -> [([(Text, Text)], Text)] -> QueryUrlCache
buildQueryUrlCache zs urlsWithAttrs =
  Map.fromList $
    catMaybes $
      urlsWithAttrs <&> \(attrs, url) -> do
        parseQueryLink attrs url >>= \case
          Left (zid, conn) -> do
            case find ((== zid) . Z.zettelID) zs of
              Nothing -> pure (url, Left $ Left (Tagged zid))
              Just z ->
                pure (url, Left $ Right (conn, z))
          Right someQ -> do
            res <- flip runReaderT zs $ runSomeZettelQuery someQ
            pure (url, Right res)