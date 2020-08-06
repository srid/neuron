{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Responsible for caching zettelkasten graph on disk
module Neuron.Web.Cache where

import Data.Aeson (eitherDecodeFileStrict, encodeFile)
import Development.Shake (Action)
import Neuron.Config.Type (Config)
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.ID (ZettelID)
import Neuron.Zettelkasten.Zettel (ZettelError)
import Relude
import Rib.Shake
import System.FilePath

data ReadMode
  = ReadMode_Direct Config
  | ReadMode_Cached
  deriving (Eq, Show)

type CacheData = (ZettelGraph, Map ZettelID ZettelError)

cacheFile :: Action FilePath
cacheFile = (</> ".neuron/cache.json") <$> ribInputDir

updateCache :: CacheData -> Action ()
updateCache v = do
  f <- cacheFile
  liftIO $ encodeFile f v

evalUnlessCacheRequested ::
  ReadMode -> (Config -> Action CacheData) -> Action CacheData
evalUnlessCacheRequested mode f = do
  case mode of
    ReadMode_Direct config ->
      f config
    ReadMode_Cached -> do
      (liftIO . eitherDecodeFileStrict =<< cacheFile) >>= \case
        Left err -> fail err
        Right v -> pure v
