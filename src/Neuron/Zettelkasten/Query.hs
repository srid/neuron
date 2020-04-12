{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Queries to the Zettel store
module Neuron.Zettelkasten.Query where

import Data.Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lucid
import Neuron.Zettelkasten.Store
import Neuron.Zettelkasten.Zettel
import Relude

-- TODO: Support querying connections, a la:
--   LinksTo ZettelID
--   LinksFrom ZettelID
data Query
  = ByTag Text
  deriving (Eq, Show)

instance ToHtml Query where
  toHtmlRaw = toHtml
  toHtml (ByTag tag) = do
    let desc = "Zettels tagged '" <> tag <> "'"
    span_ [class_ "ui basic pointing below black label", title_ desc] $ toHtml tag

instance ToHtml [Query] where
  toHtmlRaw = toHtml
  toHtml qs =
    div_ [class_ "ui horizontal divider", title_ "Zettel Query"] $ do
      toHtml `mapM_` qs

data QueryResults = QueryResults
  { resultTags :: Set.Set Text,
    resultZettels :: [Zettel]
  }

instance Semigroup QueryResults where
  QueryResults tags zettels <> QueryResults tags' zettels' =
    QueryResults (tags <> tags') (zettels <> zettels')

instance Monoid QueryResults where
  mempty = QueryResults mempty mempty

instance ToJSON QueryResults where
  toJSON QueryResults {..} =
    object
      [ "zettels" .= resultZettels,
        "tags" .= resultTags
      ]

matchQuery :: Zettel -> Query -> Bool
matchQuery Zettel {..} = \case
  ByTag tag -> tag `elem` zettelTags

matchQueries :: Zettel -> [Query] -> Bool
matchQueries zettel queries = and $ matchQuery zettel <$> queries

queryResults :: [Query] -> Zettel -> QueryResults
queryResults queries zettel
  | matchQueries zettel queries = singleResult zettel
  | otherwise = mempty

singleResult :: Zettel -> QueryResults
singleResult z@Zettel {..} = QueryResults (Set.fromList zettelTags) [z]

runQuery :: ZettelStore -> [Query] -> QueryResults
runQuery store queries =
  foldMap (queryResults queries) (Map.elems store)
