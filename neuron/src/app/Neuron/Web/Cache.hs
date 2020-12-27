{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Responsible for caching zettelkasten graph on disk
module Neuron.Web.Cache where

import Data.Aeson (eitherDecodeFileStrict, encodeFile)
import Development.Shake (Action)
import Neuron.Config.Type (Config)
import Neuron.Web.Cache.Type (NeuronCache, ReadMode (..))
import Relude
import Rib.Shake (ribOutputDir)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

cacheFile :: Action FilePath
cacheFile = do
  outputDir <- ribOutputDir
  liftIO $ createDirectoryIfMissing True outputDir
  pure $ outputDir </> "cache.json"

updateCache :: NeuronCache -> Action ()
updateCache v = do
  f <- cacheFile
  liftIO $ encodeFile f v

getCache :: Action NeuronCache
getCache = do
  (liftIO . eitherDecodeFileStrict =<< cacheFile) >>= \case
    Left err -> fail err
    Right v -> pure v

evalUnlessCacheRequested ::
  ReadMode -> (Config -> Action NeuronCache) -> Action NeuronCache
evalUnlessCacheRequested mode f = do
  case mode of
    ReadMode_Direct config ->
      f config
    ReadMode_Cached -> do
      (liftIO . eitherDecodeFileStrict =<< cacheFile) >>= \case
        Left err -> fail err
        Right v -> pure v
