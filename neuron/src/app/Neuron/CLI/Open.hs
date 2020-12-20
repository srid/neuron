{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Neuron.CLI.Open
  ( openLocallyGeneratedFile,
  )
where

import Data.Some (foldSome)
import qualified Data.Text as T
import Development.Shake (Action, doesFileExist)
import Neuron.CLI.Types (OpenCommand (..))
import Neuron.Web.Generate.Route ()
import Relude
import Rib.Route (routeUrlRel)
import Rib.Shake (ribOutputDir)
import System.FilePath ((</>))
import System.Info (os)
import System.Posix.Process (executeFile)

openLocallyGeneratedFile :: OpenCommand -> Action ()
openLocallyGeneratedFile OpenCommand {..} = do
  let relHtmlPath = T.unpack $ routeUrlRel `foldSome` route
      opener = if os == "darwin" then "open" else "xdg-open"
  htmlPath <- fmap (</> relHtmlPath) ribOutputDir
  doesFileExist htmlPath >>= \case
    False -> do
      fail "No generated HTML found. Try runing `neuron rib` first."
    True -> do
      liftIO $ executeFile opener True [htmlPath] Nothing
