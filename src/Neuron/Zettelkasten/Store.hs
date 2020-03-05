{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Graph of zettels.
module Neuron.Zettelkasten.Store where

import qualified Data.Map.Strict as Map
import Development.Shake (Action)
import Neuron.Zettelkasten.ID
import qualified Neuron.Zettelkasten.Meta as Meta
import Neuron.Zettelkasten.Type
import Path
import Relude
import qualified Rib.Parser.MMark as RibMMark

type ZettelStore = Map ZettelID Zettel

-- | Load all zettel files
mkZettelStore :: [Path Rel File] -> Action ZettelStore
mkZettelStore files = do
  zettels <- forM files $ \file -> do
    doc <- RibMMark.parse file
    let zid = mkZettelID file
        meta = Meta.getMeta doc
        title = maybe ("No title for " <> show file) Meta.title meta
        zettel = Zettel zid title doc
    pure zettel
  pure $ Map.fromList $ zettels <&> zettelID &&& id

lookupStore :: ZettelID -> ZettelStore -> Zettel
lookupStore zid = fromMaybe (error "No such zettel") . Map.lookup zid
