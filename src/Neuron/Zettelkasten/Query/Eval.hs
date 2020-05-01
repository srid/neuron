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
import Neuron.Zettelkasten.Query.Error
import Neuron.Zettelkasten.Zettel
import Relude
import Text.MMark.MarkdownLink

type family QueryResult r

type instance QueryResult (Maybe Zettel) = Zettel

type instance QueryResult [Zettel] = [Zettel]

type instance QueryResult (Map Tag Natural) = Map Tag Natural

-- | A query that is fully evaluated.
data EvaluatedQuery r = EvaluatedQuery
  { evaluatedQueryResult :: QueryResult r
  }

-- | Evaluate all queries in a zettel
--
-- Return the queries with results as a map from the original markdown link.
evalLinksInZettel ::
  MonadError QueryError m =>
  [Zettel] ->
  Zettel ->
  m (Map MarkdownLink (DSum Query Identity))
evalLinksInZettel zettels Zettel {..} =
  fmap (Map.fromList . catMaybes) $ for (extractLinks zettelContent) $ \ml ->
    fmap (ml,) <$> evalMarkdownLink zettels ml

-- | Evaluate the query in a markdown link.
evalMarkdownLink ::
  MonadError QueryError m =>
  [Zettel] ->
  MarkdownLink ->
  m (Maybe (DSum Query Identity))
evalMarkdownLink zettels ml@MarkdownLink {markdownLinkUri = uri} = liftEither $ runExcept $ do
  mq <-
    withExcept (QueryError_InvalidQuery uri) $
      queryFromMarkdownLink ml
  case mq of
    Nothing -> pure Nothing
    Just someQ -> Just <$> do
      withSome someQ $ \q ->
        pure $ q :=> Identity (runQuery zettels q)
