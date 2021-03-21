{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Neuron.CLI.Open
  ( openLocallyGeneratedFile,
  )
where

import Data.Some (foldSome)
import Neuron.CLI.Types (MonadApp, OpenCommand (..), getOutputDir)
import Neuron.Frontend.Route (routeHtmlPath)
import Relude
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.Info (os)
import System.Process

openLocallyGeneratedFile :: (MonadIO m, MonadApp m, MonadFail m) => OpenCommand -> m ()
openLocallyGeneratedFile (OpenCommand route) = do
  let relHtmlPath = routeHtmlPath `foldSome` route
      opener = case os of
                 "darwin" -> "open"
                 "mingw32" -> "cmd"
                 _ -> "xdg-open"
      extraArg = if os == "mingw32" then
                    ["/c", "start"]
                  else []
  htmlPath <- fmap (</> relHtmlPath) getOutputDir
  liftIO (doesFileExist htmlPath) >>= \case
    False -> do
      fail "No generated HTML found. Try runing `neuron gen` first."
    True -> do
      liftIO $ callProcess opener $ extraArg ++ [htmlPath]
