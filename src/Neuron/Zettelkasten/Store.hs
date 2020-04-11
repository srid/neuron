{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Zettel store datastructure
module Neuron.Zettelkasten.Store where

import qualified Data.Map.Strict as Map
import Development.Shake (Action)
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Zettel
import Relude
import Rib (MMark)

type ZettelStore = Map ZettelID (Zettel MMark)

-- | Load all zettel files
mkZettelStore :: [FilePath] -> Action ZettelStore
mkZettelStore files = do
  zettels <- mkZettelFromPath `mapM` files
  pure $ Map.fromList $ zettels <&> zettelID &&& id

lookupStore :: ZettelID -> ZettelStore -> Zettel MMark
lookupStore zid = fromMaybe (error $ "No such zettel: " <> zettelIDText zid) . Map.lookup zid
