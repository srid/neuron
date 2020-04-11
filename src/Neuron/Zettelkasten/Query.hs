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

import qualified Data.Map.Strict as Map
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
    span_ [class_ "ui basic pointing below label", title_ desc] $ toHtml tag

instance ToHtml [Query] where
  toHtmlRaw = toHtml
  toHtml qs =
    div_ [class_ "ui horizontal divider", title_ "Zettel Query"] $ do
      toHtml `mapM_` qs

matchQuery :: Zettel -> Query -> Bool
matchQuery Zettel {..} = \case
  ByTag tag -> tag `elem` zettelTags

runQuery :: ZettelStore -> [Query] -> [Zettel]
runQuery store queries =
  flip filter (Map.elems store) $ \z ->
    and $ matchQuery z <$> queries
