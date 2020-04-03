{-# LANGUAGE DeriveGeneric #-}
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
import Neuron.Zettelkasten.ID
import qualified Neuron.Zettelkasten.Meta as Meta
import Neuron.Zettelkasten.Store
import Neuron.Zettelkasten.Type
import Relude

-- TODO: Support querying connections, a la:
--   LinksTo ZettelID
--   LinksFrom ZettelID
data Query
  = ByTag Text
  deriving (Eq, Show)

data Match
  = Match
      { matchID :: ZettelID,
        matchTitle :: Text,
        matchTags :: [Text]
      }

-- TODO: Use generic deriving use field label modifier.
instance ToJSON Match where
  toJSON Match {..} =
    object
      [ "id" .= toJSON matchID,
        "title" .= matchTitle,
        "tags" .= matchTags
      ]

matchQuery :: Match -> Query -> Bool
matchQuery Match {..} = \case
  ByTag tag -> tag `elem` matchTags

extractMatch :: Zettel -> Maybe Match
extractMatch Zettel {..} = do
  Meta.Meta {..} <- Meta.getMeta zettelContent
  pure
    Match
      { matchID = zettelID,
        matchTitle = zettelTitle,
        matchTags = fromMaybe [] tags
      }

runQuery :: ZettelStore -> [Query] -> [Match]
runQuery store queries =
  flip filter database $ \match -> and $ matchQuery match <$> queries
  where
    database = catMaybes $ extractMatch <$> Map.elems store
