{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Neuron.CLI.Open
  ( openLocallyGeneratedFile,
  )
where

import Data.Some (foldSome)
import qualified Data.Text as T
import Neuron.CLI.Types (MonadApp, OpenCommand (..), getOutputDir)
import Neuron.Web.Generate.Route ()
import Relude
import Rib.Route (routeUrlRel)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.Info (os)
import System.Posix.Process (executeFile)

openLocallyGeneratedFile :: (MonadIO m, MonadApp m, MonadFail m) => OpenCommand -> m ()
openLocallyGeneratedFile OpenCommand {..} = do
  let relHtmlPath = T.unpack $ routeUrlRel `foldSome` route
      opener = if os == "darwin" then "open" else "xdg-open"
  htmlPath <- fmap (</> relHtmlPath) getOutputDir
  liftIO (doesFileExist htmlPath) >>= \case
    False -> do
      fail "No generated HTML found. Try runing `neuron rib` first."
    True -> do
      liftIO $ executeFile opener True [htmlPath] Nothing
