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

queryZettelkasten :: FilePath -> Some Query -> Action ()
queryZettelkasten notesDir =
  foldSome $ \q -> do
    result <- flip runQuery q <$> G.loadZettels
    putLTextLn $ Aeson.encodeToLazyText $ queryResultJson notesDir q result
