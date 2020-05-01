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
import Data.Traversable (for)
import Neuron.Zettelkasten.Query
import Neuron.Zettelkasten.Query.Error
import Neuron.Zettelkasten.Zettel
import Relude
import Text.MMark.MarkdownLink

-- | Evaluate all queries in a zettel
--
-- Return the queries with results as a map from the original markdown link.
evalLinksInZettel ::
  MonadError QueryParseError m =>
  [Zettel] ->
  Zettel ->
  m (Map MarkdownLink (DSum Query Identity))
evalLinksInZettel zettels Zettel {..} =
  fmap (Map.fromList . catMaybes) $ for (extractLinks zettelContent) $ \ml ->
    fmap (ml,) <$> evalMarkdownLink zettels ml

-- | Evaluate the query in a markdown link.
evalMarkdownLink ::
  MonadError QueryParseError m =>
  [Zettel] ->
  MarkdownLink ->
  m (Maybe (DSum Query Identity))
evalMarkdownLink zettels ml =
  liftEither $ runExcept $ do
    mq <- queryFromMarkdownLink ml
    case mq of
      Nothing -> pure Nothing
      Just someQ -> Just <$> do
        withSome someQ $ \q ->
          pure $ q :=> Identity (runQuery zettels q)
