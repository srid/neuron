{-# LANGUAGE RecordWildCards #-}

module Neuron.CLI.Open
  ( openLocallyGeneratedFile,
  )
where

import Data.Some
import qualified Data.Text as T
import Development.Shake (Action)
import Neuron.CLI.Types (OpenCommand (..))
import Neuron.Web.Generate.Route ()
import Relude
import Rib.Route (routeUrlRel)
import Rib.Shake (ribOutputDir)
import System.FilePath
import System.Info (os)
import System.Posix.Process

openLocallyGeneratedFile :: OpenCommand -> Action ()
openLocallyGeneratedFile OpenCommand {..} = do
  let htmlFile = T.unpack $ routeUrlRel `foldSome` route
  indexHtmlPath <- fmap (</> htmlFile) ribOutputDir
  let opener = if os == "darwin" then "open" else "xdg-open"
  liftIO $ executeFile opener True [indexHtmlPath] Nothing
