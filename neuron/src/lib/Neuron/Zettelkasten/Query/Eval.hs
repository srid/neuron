{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Query.Eval where

import Control.Monad.Writer
import qualified Data.Map.Strict as Map
import Data.Tagged
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.Query.Parser (parseQueryLink)
import Neuron.Zettelkasten.Zettel
  ( MissingZettel,
    Zettel,
  )
import qualified Neuron.Zettelkasten.Zettel as Z
import Relude

-- TODO move to WikiLinkjs plugin
queryConnectionsFromWikiLink ::
  forall m.
  ( -- Running queries requires the zettels list.
    MonadReader [Zettel] m,
    -- Track missing zettel links in writer
    MonadWriter [MissingZettel] m
  ) =>
  Zettel ->
  m [(ContextualConnection, Zettel)]
queryConnectionsFromWikiLink z = do
  zs <- ask
  fmap concat $
    forM (Z.zettelQueries z) $ \((zid, conn), ctx) -> do
      case find ((== zid) . Z.zettelID) zs of
        Nothing -> pure mempty
        Just z2 -> do
          pure [((conn, ctx), z2)]

type QueryUrlCache = Map Text (Either MissingZettel (Connection, Zettel))

buildQueryUrlCache :: [Zettel] -> [([(Text, Text)], Text)] -> QueryUrlCache
buildQueryUrlCache zs urlsWithAttrs =
  Map.fromList $
    catMaybes $
      urlsWithAttrs <&> \(attrs, url) -> do
        (zid, conn) <- parseQueryLink attrs url
        case find ((== zid) . Z.zettelID) zs of
          Nothing -> pure (url, Left (Tagged zid))
          Just z ->
            pure (url, Right (conn, z))