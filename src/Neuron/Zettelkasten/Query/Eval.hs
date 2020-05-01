{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Query.Eval
  ( evalZettelLinks,
  )
where

import Control.Monad.Except
import Data.Dependent.Sum
import qualified Data.Map.Strict as Map
import Data.Some
import Data.Traversable (for)
import Lucid
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.Query
import Neuron.Zettelkasten.Query.Error
import Neuron.Zettelkasten.Query.View (renderQueryLink)
import Neuron.Zettelkasten.Zettel
import Relude
import Text.MMark.MarkdownLink

type EvalResult = ([(Connection, Zettel)], Html ())

-- | Evaluate all queries in a zettel
--
-- Return the queries with results as a map from the original markdown link.
evalZettelLinks ::
  MonadError QueryError m =>
  [Zettel] ->
  Zettel ->
  m (Map MarkdownLink EvalResult)
evalZettelLinks zettels Zettel {..} =
  fmap (Map.fromList . catMaybes) $ for (extractLinks zettelContent) $ \ml ->
    fmap (ml,) <$> evalMarkdownLink zettels ml

-- | Fully evaluate the query in a markdown link.
evalMarkdownLink ::
  MonadError QueryError m =>
  [Zettel] ->
  MarkdownLink ->
  m (Maybe EvalResult)
evalMarkdownLink zettels ml =
  liftEither $ runExcept $ do
    mq <- withExcept Left $ queryFromMarkdownLink ml
    case mq of
      Nothing -> pure Nothing
      Just someQ -> Just <$> do
        let qres = withSome someQ $ \q ->
              q :=> Identity (runQuery zettels q)
            conns = getConnections qres
        view <-
          withExcept Right $
            renderQueryLink qres
        pure (conns, view)
  where
    getConnections :: DSum Query Identity -> [(Connection, Zettel)]
    getConnections = \case
      Query_ZettelByID _ mconn :=> Identity mres ->
        maybe [] pure $ (fromMaybe Folgezettel mconn,) <$> mres
      Query_ZettelsByTag _ mconn _mview :=> Identity res ->
        (fromMaybe Folgezettel mconn,) <$> res
      _ ->
        []
