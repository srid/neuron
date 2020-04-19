{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Special Zettel links in Markdown
module Neuron.Zettelkasten.Link where

import Control.Monad.Except
import Data.Some
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Link.Theme
import Neuron.Zettelkasten.Markdown (MarkdownLink (..))
import Neuron.Zettelkasten.Query (Query (..), queryFromMarkdownLink, runQuery)
import Neuron.Zettelkasten.Store
import Neuron.Zettelkasten.Tag
import Neuron.Zettelkasten.Zettel
import Relude
import qualified Text.URI as URI

type family QueryConnection q

type instance QueryConnection Zettel = Connection

type instance QueryConnection [Zettel] = Connection

type instance QueryConnection [Tag] = ()

type family QueryViewTheme q

type instance QueryViewTheme Zettel = LinkTheme

type instance QueryViewTheme [Zettel] = LinkTheme

type instance QueryViewTheme [Tag] = ()

-- TODO: Refactor to be a GADT using Some, and derive GEq, etc. correctly
data NeuronLink
  = forall r.
    (Show (Query r), Show (QueryConnection r), Show (QueryViewTheme r)) =>
    NeuronLink (Query r, QueryConnection r, QueryViewTheme r)

deriving instance Show NeuronLink

instance Eq NeuronLink where
  (==) (NeuronLink (Query_ZettelByID zid1, c1, t1)) (NeuronLink (Query_ZettelByID zid2, c2, t2)) =
    and [zid1 == zid2, c1 == c2, t1 == t2]
  (==) (NeuronLink (Query_ZettelsByTag p1, c1, t1)) (NeuronLink (Query_ZettelsByTag p2, c2, t2)) =
    and [p1 == p2, c1 == c2, t1 == t2]
  (==) (NeuronLink (Query_Tags p1, c1, t1)) (NeuronLink (Query_Tags p2, c2, t2)) =
    and [p1 == p2, c1 == c2, t1 == t2]
  (==) _ _ =
    False

neuronLinkFromMarkdownLink :: MonadError Text m => MarkdownLink -> m (Maybe NeuronLink)
neuronLinkFromMarkdownLink ml@MarkdownLink {markdownLinkUri = uri} = do
  queryFromMarkdownLink ml >>= \case
    Nothing -> pure Nothing
    Just someQ -> Just <$> do
      withSome someQ $ \q -> case q of
        Query_ZettelByID _ ->
          pure $ NeuronLink (q, connectionFromURI uri, linkThemeFromURI uri)
        Query_ZettelsByTag _ ->
          pure $ NeuronLink (q, connectionFromURI uri, linkThemeFromURI uri)
        Query_Tags _ ->
          pure $ NeuronLink (q, (), ())

neuronLinkConnections :: ZettelStore -> NeuronLink -> [(Connection, ZettelID)]
neuronLinkConnections store = \case
  NeuronLink (Query_ZettelByID zid, conn, _) ->
    [(conn, zid)]
  NeuronLink (q@(Query_ZettelsByTag _pats), conn, _) ->
    (conn,) . zettelID <$> runQuery store q
  _ ->
    []

connectionFromURI :: URI.URI -> Connection
connectionFromURI uri =
  fromMaybe Folgezettel $
    case fmap URI.unRText (URI.uriScheme uri) of
      Just scheme
        | scheme `elem` ["zcf", "zcfquery"] ->
          Just OrdinaryConnection
      _ ->
        Nothing
