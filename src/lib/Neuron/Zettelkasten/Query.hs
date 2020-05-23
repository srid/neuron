{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Queries to the Zettel store
module Neuron.Zettelkasten.Query where

import Control.Monad.Except
import Data.Aeson
import Data.Aeson.GADT.TH
import Data.GADT.Compare.TH
import Data.GADT.Show.TH
import qualified Data.Map.Strict as Map
import Data.TagTree (Tag, TagPattern (..), tagMatch, tagMatchAny, tagTree)
import Data.Tree (Tree (..))
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Query.Error
import Neuron.Zettelkasten.Query.Theme
import Neuron.Zettelkasten.Zettel
import Relude
import System.FilePath

-- | Query represents a way to query the Zettelkasten.
data Query r where
  Query_ZettelByID :: ZettelID -> Maybe Connection -> Query (Maybe Zettel)
  Query_ZettelsByTag :: [TagPattern] -> Maybe Connection -> ZettelsView -> Query [Zettel]
  Query_Tags :: [TagPattern] -> Query (Map Tag Natural)

-- | Run the given query and return the results.
runQuery :: [Zettel] -> Query r -> r
runQuery zs = \case
  Query_ZettelByID zid _ ->
    find ((== zid) . zettelID) zs
  Query_ZettelsByTag pats _mconn _mview ->
    sortZettelsReverseChronological $ flip filter zs $ \Zettel {..} ->
      and $ flip fmap pats $ \pat ->
        any (tagMatch pat) zettelTags
  Query_Tags [] ->
    allTags
  Query_Tags pats ->
    Map.filterWithKey (const . tagMatchAny pats) allTags
  where
    allTags :: Map.Map Tag Natural
    allTags =
      Map.fromListWith (+) $
        concatMap (\Zettel {..} -> (,1) <$> zettelTags) zs

queryResultJson ::
  forall r.
  (ToJSON (Query r)) =>
  FilePath ->
  Query r ->
  r ->
  Map ZettelID [QueryParseError] ->
  Value
queryResultJson notesDir q r errors =
  toJSON $
    object
      [ "query" .= toJSON q,
        "result" .= resultJson,
        "errors" .= errors
      ]
  where
    resultJson :: Value
    resultJson = case q of
      Query_ZettelByID _ _mconn ->
        toJSON $ zettelJsonFull <$> r
      Query_ZettelsByTag _ _mconn _mview ->
        toJSON $ zettelJsonFull <$> r
      Query_Tags _ ->
        toJSON $ treeToJson <$> tagTree r
    zettelJsonFull :: ZettelT c -> Value
    zettelJsonFull z@Zettel {..} =
      object $
        [ "path" .= (notesDir </> zettelIDSourceFileName zettelID)
        ]
          <> zettelJson z
    treeToJson (Node (tag, count) children) =
      object
        [ "name" .= tag,
          "count" .= count,
          "children" .= fmap treeToJson children
        ]

deriveJSONGADT ''Query

deriveGEq ''Query

deriveGShow ''Query

deriving instance Show (Query (Maybe Zettel))

deriving instance Show (Query [Zettel])

deriving instance Show (Query (Map Tag Natural))

deriving instance Eq (Query (Maybe Zettel))

deriving instance Eq (Query [Zettel])

deriving instance Eq (Query (Map Tag Natural))
