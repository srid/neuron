{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Query.Eval where

import Control.Monad.Except
import Data.Dependent.Sum
import qualified Data.Map.Strict as Map
import Data.Some
import Data.TagTree (Tag)
import Data.Traversable (for)
import Neuron.Zettelkasten.Query
import Neuron.Zettelkasten.Query.Connection
import Neuron.Zettelkasten.Query.Error
import Neuron.Zettelkasten.Query.Theme
import Neuron.Zettelkasten.Zettel
import Relude
import Text.MMark.MarkdownLink

type family QueryResult r

type instance QueryResult (Maybe Zettel) = Zettel

type instance QueryResult [Zettel] = [Zettel]

type instance QueryResult (Map Tag Natural) = Map Tag Natural

-- | A query that is fully evaluated.
data EvaluatedQuery r = EvaluatedQuery
  { evaluatedQueryResult :: QueryResult r,
    evaluatedQueryConnection :: QueryConnection r,
    evaluatedQueryTheme :: QueryTheme r
  }

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
