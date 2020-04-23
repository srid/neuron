{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Special Zettel links in Markdown
module Neuron.Zettelkasten.Link where

import Control.Monad.Except
import Data.Dependent.Sum
import qualified Data.Map.Strict as Map
import Data.Some
import Data.TagTree (Tag)
import Data.Traversable (for)
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Link.Theme
import Neuron.Zettelkasten.Query (InvalidQuery (..), Query (..), queryFromMarkdownLink, runQuery)
import Neuron.Zettelkasten.Zettel
import Relude
import Text.MMark.MarkdownLink
import qualified Text.URI as URI
import Text.URI (URI)

type family QueryResult r

type instance QueryResult (Maybe Zettel) = Zettel

type instance QueryResult [Zettel] = [Zettel]

type instance QueryResult (Map Tag Natural) = Map Tag Natural

type family QueryConnection q

type instance QueryConnection (Maybe Zettel) = Connection

type instance QueryConnection [Zettel] = Connection

type instance QueryConnection (Map Tag Natural) = ()

type family QueryViewTheme q

type instance QueryViewTheme (Maybe Zettel) = ZettelView

type instance QueryViewTheme [Zettel] = ZettelsView

type instance QueryViewTheme (Map Tag Natural) = ()

-- | A query that is fully evaluated.
data EvaluatedQuery r = EvaluatedQuery
  { evaluatedQueryResult :: QueryResult r,
    evaluatedQueryConnection :: QueryConnection r,
    evaluatedQueryViewTheme :: QueryViewTheme r
  }

type ZettelQueryResource = Map MarkdownLink (DSum Query EvaluatedQuery)

-- | Evaluate all queries in a zettel
--
-- Return the queries with results as a map from the original markdown link.
evaluateQueries ::
  MonadError QueryError m =>
  [Zettel] ->
  Zettel ->
  m (Map MarkdownLink (DSum Query EvaluatedQuery))
evaluateQueries zettels Zettel {..} =
  fmap (Map.fromList . catMaybes) $ for (extractLinks zettelContent) $ \ml ->
    fmap (ml,) <$> evaluateQuery zettels ml

-- | Evaluate the query in a markdown link.
evaluateQuery ::
  MonadError QueryError m =>
  [Zettel] ->
  MarkdownLink ->
  m (Maybe (DSum Query EvaluatedQuery))
evaluateQuery zettels ml@MarkdownLink {markdownLinkUri = uri} = liftEither $ runExcept $ do
  mq <-
    withExcept (QueryError_InvalidQuery uri) $
      queryFromMarkdownLink ml
  case mq of
    Nothing -> pure Nothing
    Just someQ -> Just <$> do
      withSome someQ $ \case
        q@(Query_ZettelByID zid) -> do
          viewTheme <-
            withExcept (QueryError_InvalidQueryView uri) $
              linkThemeFromURI uri
          let conn = connectionFromURI uri
          case runQuery zettels q of
            Nothing ->
              throwError $ QueryError_ZettelNotFound uri zid
            Just zettel ->
              pure $ q :=> EvaluatedQuery zettel conn viewTheme
        q@(Query_ZettelsByTag _pats) -> do
          viewTheme <-
            withExcept (QueryError_InvalidQueryView uri) $
              zettelsViewFromURI uri
          let conn = connectionFromURI uri
          pure $ q :=> EvaluatedQuery (runQuery zettels q) conn viewTheme
        q@(Query_Tags _filters) ->
          pure $ q :=> EvaluatedQuery (runQuery zettels q) () ()

data QueryError
  = QueryError_InvalidQuery URI InvalidQuery
  | QueryError_InvalidQueryView URI InvalidLinkTheme
  | QueryError_ZettelNotFound URI ZettelID
  deriving (Eq, Show)

queryErrorUri :: QueryError -> URI
queryErrorUri = \case
  QueryError_InvalidQuery uri _ -> uri
  QueryError_InvalidQueryView uri _ -> uri
  QueryError_ZettelNotFound uri _ -> uri

connectionFromURI :: URI.URI -> Connection
connectionFromURI uri =
  fromMaybe Folgezettel $
    case fmap URI.unRText (URI.uriScheme uri) of
      Just scheme
        | scheme `elem` ["zcf", "zcfquery"] ->
          Just OrdinaryConnection
      _ ->
        Nothing
