{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.CLI.Query
  ( queryZettelkasten,
  )
where

import qualified Data.Aeson.Text as Aeson
import Data.Some
import Development.Shake (Action)
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.Query (Query (..), queryResultJson, runQuery)
import Relude
import qualified Rib

queryZettelkasten :: FilePath -> Some Query -> Action ()
queryZettelkasten notesDir query = do
  zettels <- fmap (fmap fst . snd) . G.loadZettelkasten =<< Rib.forEvery ["*.md"] pure
  withSome query $ \q -> do
    let result = runQuery zettels q
    putLTextLn $ Aeson.encodeToLazyText $ queryResultJson notesDir q result
