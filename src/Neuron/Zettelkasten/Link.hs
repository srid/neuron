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
import Neuron.Zettelkasten.Query (InvalidQuery (..), Query (..), queryFromMarkdownLink, runQuery)
import Neuron.Zettelkasten.Tag
import Neuron.Zettelkasten.Zettel
import Relude
import qualified Text.URI as URI
import Text.URI (URI)

type family QueryConnection q

type instance QueryConnection (Maybe Zettel) = Connection

type instance QueryConnection [Zettel] = Connection

type instance QueryConnection (Map Tag Natural) = ()

type family QueryViewTheme q

type instance QueryViewTheme (Maybe Zettel) = ZettelView

type instance QueryViewTheme [Zettel] = ZettelsView

type instance QueryViewTheme (Map Tag Natural) = ()

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

data InvalidNeuronLink
  = InvalidNeuronLink URI (Either InvalidQuery InvalidLinkTheme)
  deriving (Eq, Show)

neuronLinkFromMarkdownLink :: MonadError InvalidNeuronLink m => MarkdownLink -> m (Maybe NeuronLink)
neuronLinkFromMarkdownLink ml@MarkdownLink {markdownLinkUri = uri} = liftEither $ runExcept $ do
  l <- withExcept (InvalidNeuronLink uri . Left) $ queryFromMarkdownLink ml
  case l of
    Nothing -> pure Nothing
    Just someQ -> Just <$> do
      withSome someQ $ \q -> case q of
        Query_ZettelByID _ -> do
          t <- withExcept (InvalidNeuronLink uri . Right) $ linkThemeFromURI uri
          pure $ NeuronLink (q, connectionFromURI uri, t)
        Query_ZettelsByTag _ -> do
          t <- withExcept (InvalidNeuronLink uri . Right) $ zettelsViewFromURI uri
          pure $ NeuronLink (q, connectionFromURI uri, t)
        Query_Tags _ ->
          pure $ NeuronLink (q, (), ())

neuronLinkConnections :: [Zettel] -> NeuronLink -> [(Connection, ZettelID)]
neuronLinkConnections zettels = \case
  NeuronLink (Query_ZettelByID zid, conn, _) ->
    [(conn, zid)]
  NeuronLink (q@(Query_ZettelsByTag _pats), conn, _) ->
    (conn,) . zettelID <$> runQuery zettels q
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
