{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Queries to the Zettel store
module Neuron.Zettelkasten.Query where

import Data.Aeson
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lucid
import Neuron.Zettelkasten.Store
import Neuron.Zettelkasten.Tag
import Neuron.Zettelkasten.Zettel
import Relude
import qualified Text.URI as URI

-- TODO: Support querying connections, a la:
--   LinksTo ZettelID
--   LinksFrom ZettelID
data Query
  = ByTag Tag
  | TagUnder Tag
  | TagFrom Tag
  | TagGlob TagPattern
  deriving (Eq, Show)

instance ToHtml Query where
  toHtmlRaw = toHtml
  toHtml query =
    let (desc, repr) = case query of
          ByTag (tagToText -> tag) -> ("Zettels tagged '" <> tag <> "'", tag)
          TagUnder (tagToText -> tag) -> ("Zettels under tag '" <> tag <> "'", tag)
          TagFrom (tagToText -> tag) -> ("Zettels under tag '" <> tag <> "' (included)", tag)
          TagGlob (tagPatternToText -> pat) -> ("Zettels matching pattern '" <> pat <> "'", pat)
     in span_ [class_ "ui basic pointing below black label", title_ desc] $ toHtml repr

instance ToHtml [Query] where
  toHtmlRaw = toHtml
  toHtml qs =
    div_ [class_ "ui horizontal divider", title_ "Zettel Query"] $ do
      if null qs
        then "All zettels"
        else toHtml `mapM_` qs

data QueryResults = QueryResults
  { resultTags :: Set.Set Tag,
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

parseQuery :: URI.URI -> [Query]
parseQuery uri =
  flip mapMaybe (URI.uriQuery uri) $ \case
    URI.QueryParam (URI.unRText -> key) (URI.unRText -> val) ->
      case key of
        "tag" -> pure $ ByTag (Tag val)
        "under" -> pure $ TagUnder (Tag val)
        "from" -> pure $ TagFrom (Tag val)
        "glob" -> pure $ TagGlob (TagPattern $ toString val)
        _ -> Nothing
    _ -> Nothing

queryToTagPattern :: Query -> TagPattern
queryToTagPattern = \case
  ByTag tag -> literalPattern tag
  TagUnder tag -> literalPattern tag <> TagPattern "**"
  TagFrom tag -> literalPattern tag <> TagPattern "*" <> TagPattern "**"
  TagGlob pat -> pat

matchQuery :: Zettel -> Query -> Bool
matchQuery Zettel {..} query = any (tagMatch $ queryToTagPattern query) zettelTags

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
  foldMap toQueryResult $
    tagMatchMany
      (((),) . queryToTagPattern <$> queries)
      (foldMap associateTags $ Map.elems store)
  where
    associateTags z@Zettel {..} = (z,) <$> zettelTags
    toQueryResult = singleResult . snd
