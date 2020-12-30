{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Responsible for caching zettelkasten graph on disk
module Neuron.Web.Cache where

import Data.Aeson (eitherDecodeFileStrict, encodeFile)
import Neuron.CLI.Types (MonadApp, getOutputDir)
import Neuron.Config.Type (Config)
import Neuron.Web.Cache.Type (NeuronCache, ReadMode (..))
import Relude
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

cacheFile :: (MonadIO m, MonadApp m) => m FilePath
cacheFile = do
  outputDir <- getOutputDir
  liftIO $ createDirectoryIfMissing True outputDir
  pure $ outputDir </> "cache.json"

updateCache :: (MonadIO m, MonadApp m) => NeuronCache -> m ()
updateCache v = do
  f <- cacheFile
  liftIO $ encodeFile f v

getCache :: (MonadIO m, MonadApp m, MonadFail m) => m NeuronCache
getCache = do
  (liftIO . eitherDecodeFileStrict =<< cacheFile) >>= \case
    Left err -> fail err
    Right v -> pure v

evalUnlessCacheRequested ::
  (MonadIO m, MonadFail m, MonadApp m) => ReadMode -> (Config -> m NeuronCache) -> m NeuronCache
evalUnlessCacheRequested mode f = do
  case mode of
    ReadMode_Direct config ->
      f config
    ReadMode_Cached -> do
      (liftIO . eitherDecodeFileStrict =<< cacheFile) >>= \case
        Left err -> fail err
        Right v -> pure v
