{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Queries to the Zettel store
module Neuron.Zettelkasten.Query where

import Control.Monad.Except
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import qualified Data.Map.Strict as Map
import Data.Some
import Lucid
import Neuron.Zettelkasten.Store
import Neuron.Zettelkasten.Tag
import Neuron.Zettelkasten.Zettel
import Neuron.Zettelkasten.ID
import Relude
import qualified Text.URI as URI

-- | Query represents a way to query the Zettelkasten.
--
-- TODO: Support querying connections, a la:
--   LinksTo ZettelID
--   LinksFrom ZettelID
data Query r where
  Query_ZettelByID :: ZettelID -> Query Zettel
  Query_ZettelsByTag :: [TagPattern] -> Query [Zettel]
  Query_Tags :: [TagPattern] -> Query [Tag]

deriveGEq ''Query

deriveGShow ''Query

deriving instance Show (Query [Zettel])

deriving instance Eq (Query [Zettel])


instance ToHtml (Query [Zettel]) where
  toHtmlRaw = toHtml
  toHtml = \case
    Query_ZettelsByTag (fmap unTagPattern -> pats) ->
      div_ [class_ "ui horizontal divider", title_ "Zettel Query"] $ do
        if null pats
          then "All zettels"
          else
            let desc = "Zettels tagged '" <> show pats <> "'"
             in span_ [class_ "ui basic pointing below black label", title_ desc] $ toHtml $ show @Text pats

type QueryResults = [Zettel]

queryFromURI :: MonadError Text m => URI.URI -> m (Some Query)
queryFromURI uri =
  case fmap URI.unRText (URI.uriScheme uri) of
    Just proto | proto `elem` ["zquery", "zcfquery"] ->
      pure $ Some $ Query_ZettelsByTag $ flip mapMaybe (URI.uriQuery uri) $ \case
        URI.QueryParam (URI.unRText -> key) (URI.unRText -> val) ->
          case key of
            "tag" -> Just (TagPattern $ toString val)
            _ -> Nothing
        _ -> Nothing
    _ -> throwError "Bad URI (expected: zquery: or zcfquery:)"

-- | Run the given query and return the results.
runQuery :: ZettelStore -> Query r -> r
runQuery store = \case
  Query_ZettelByID zid ->
    lookupStore zid store
  Query_ZettelsByTag pats ->
    foldMap (queryResults pats) (Map.elems store)
  Query_Tags _pats ->
    -- TODO:
    []

matchQuery :: Zettel -> TagPattern -> Bool
matchQuery Zettel {..} pat =
  any (tagMatch pat) zettelTags

matchQueries :: Zettel -> [TagPattern] -> Bool
matchQueries zettel pats = and $ matchQuery zettel <$> pats

queryResults :: [TagPattern] -> Zettel -> [Zettel]
queryResults pats zettel
  | matchQueries zettel pats = [zettel]
  | otherwise = mempty
