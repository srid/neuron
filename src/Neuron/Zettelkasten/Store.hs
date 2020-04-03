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
import qualified Data.Text.IO as Text
import Development.Shake (Action)
import Neuron.Zettelkasten.ID
import qualified Neuron.Zettelkasten.Meta as Meta
import Neuron.Zettelkasten.Type
import Path
import Relude
import qualified Rib.Parser.MMark as RibMMark

type ZettelStore = Map ZettelID Zettel

-- | Load all zettel files
mkZettelStoreWith :: Monad m => (Path Rel File -> m RibMMark.MMark) -> [Path Rel File] -> m ZettelStore
mkZettelStoreWith parse files = do
  zettels <- forM files $ \file -> do
    doc <- parse file
    let zid = mkZettelID file
        meta = Meta.getMeta doc
        title = maybe ("No title for " <> show file) Meta.title meta
        zettel = Zettel zid title doc
    pure zettel
  pure $ Map.fromList $ zettels <&> zettelID &&& id

mkZettelStore :: [Path Rel File] -> Action ZettelStore
mkZettelStore = mkZettelStoreWith RibMMark.parse

mkZettelStoreIO :: Path Abs Dir -> [Path Rel File] -> IO ZettelStore
mkZettelStoreIO inputDir = mkZettelStoreWith $ \file -> do
  let path = toFilePath $ inputDir </> file
  src <- Text.readFile path
  case RibMMark.parsePure path src of
    Right doc -> pure doc
    Left err -> fail (toString err)

lookupStore :: ZettelID -> ZettelStore -> Zettel
lookupStore zid = fromMaybe (error $ "No such zettel: " <> unZettelID zid) . Map.lookup zid
