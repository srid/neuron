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

import qualified Data.Map.Strict as Map
import Neuron.Zettelkasten.Store
import Neuron.Zettelkasten.Zettel
import Relude

-- TODO: Support querying connections, a la:
--   LinksTo ZettelID
--   LinksFrom ZettelID
data Query
  = ByTag Text
  deriving (Eq, Show)

matchQuery :: Zettel () -> Query -> Bool
matchQuery Zettel {..} = \case
  ByTag tag -> tag `elem` zettelTags

runQuery :: ZettelStore -> [Query] -> [Zettel ()]
runQuery store queries =
  flip filter zettels $ \z -> and $ matchQuery z <$> queries
  where
    zettels = fmap (const ()) <$> Map.elems store
