{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Queries to the Zettel store
module Neuron.Zettelkasten.Query where

import Control.Monad.Except
import qualified Data.Map.Strict as Map
import Lucid
import Neuron.Zettelkasten.Store
import Neuron.Zettelkasten.Tag
import Neuron.Zettelkasten.Zettel
import Relude
import qualified Text.URI as URI

-- | Query represents a way to query the Zettelkasten.
--
-- TODO: Support querying connections, a la:
--   LinksTo ZettelID
--   LinksFrom ZettelID
data Query
  = Query_ZettelsByTag TagPattern
  deriving (Eq, Show)

instance ToHtml Query where
  toHtmlRaw = toHtml
  toHtml (Query_ZettelsByTag (TagPattern pat)) =
    let desc = "Zettels matching tag '" <> toText pat <> "'"
     in span_ [class_ "ui basic pointing below black label", title_ desc] $ toHtml pat

instance ToHtml [Query] where
  toHtmlRaw = toHtml
  toHtml qs =
    div_ [class_ "ui horizontal divider", title_ "Zettel Query"] $ do
      if null qs
        then "All zettels"
        else toHtml `mapM_` qs

type QueryResults = [Zettel]

queryFromURI :: MonadError Text m => URI.URI -> m [Query]
queryFromURI uri =
  case fmap URI.unRText (URI.uriScheme uri) of
    Just proto | proto `elem` ["zquery", "zcfquery"] ->
      pure $ flip mapMaybe (URI.uriQuery uri) $ \case
        URI.QueryParam (URI.unRText -> key) (URI.unRText -> val) ->
          case key of
            "tag" -> Just $ Query_ZettelsByTag (TagPattern $ toString val)
            _ -> Nothing
        _ -> Nothing
    _ -> throwError "Bad URI (expected: zquery: or zcfquery:)"

matchQuery :: Zettel -> Query -> Bool
matchQuery Zettel {..} = \case
  Query_ZettelsByTag pat -> any (tagMatch pat) zettelTags

matchQueries :: Zettel -> [Query] -> Bool
matchQueries zettel queries = and $ matchQuery zettel <$> queries

queryResults :: [Query] -> Zettel -> QueryResults
queryResults queries zettel
  | matchQueries zettel queries = [zettel]
  | otherwise = mempty

-- | Run the given query and return the results.
runQuery :: ZettelStore -> [Query] -> QueryResults
runQuery store queries =
  foldMap (queryResults queries) (Map.elems store)
