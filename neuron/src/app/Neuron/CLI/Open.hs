{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards   #-}

module Neuron.CLI.Open
  ( openLocallyGeneratedFile,
  )
where

import qualified Data.Text as T
import Development.Shake (Action)
import Neuron.CLI.Types (OpenCommand (..))
import Relude
import Rib.Shake (ribOutputDir)
import System.FilePath
import System.Info (os)
import System.Posix.Process

openLocallyGeneratedFile :: OpenCommand -> Action ()
openLocallyGeneratedFile OpenCommand {..} = do
  let htmlId = maybe "index" T.unpack zettelId
  indexHtmlPath <- fmap (</> (htmlId ++ ".html")) ribOutputDir
  let opener = if os == "darwin" then "open" else "xdg-open"
  liftIO $ executeFile opener True [indexHtmlPath] Nothing
