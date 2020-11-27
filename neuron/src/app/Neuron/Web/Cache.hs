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
import Rib.Shake (ribInputDir)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

data ReadMode
  = ReadMode_Direct Config
  | ReadMode_Cached
  deriving (Eq, Show)

type CacheData = (ZettelGraph, Map ZettelID (NonEmpty ZettelError))

cacheFile :: Action FilePath
cacheFile = do
  neuronDir <- (</> ".neuron") <$> ribInputDir
  liftIO $ createDirectoryIfMissing True neuronDir
  pure $ neuronDir </> "cache.json"

updateCache :: CacheData -> Action ()
updateCache v = do
  f <- cacheFile
  liftIO $ encodeFile f v

getCache :: Action CacheData
getCache = do
  (liftIO . eitherDecodeFileStrict =<< cacheFile) >>= \case
    Left err -> fail err
    Right v -> pure v

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
