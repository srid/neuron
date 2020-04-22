{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Main module for using neuron as a library, instead of as a CLI tool.
module Neuron.Web.Generate
  ( generateSite,
  )
where

import qualified Data.Map.Strict as Map
import Development.Shake (Action)
import qualified Neuron.Config as Z
import Neuron.Version (neuronVersion, olderThan)
import qualified Neuron.Web.Route as Z
import qualified Neuron.Zettelkasten.Graph as Z
import qualified Neuron.Zettelkasten.Store as Z
import Options.Applicative
import Relude
import qualified Rib

-- | Generate the Zettelkasten site
generateSite ::
  Z.Config ->
  (forall a. Z.Route Z.ZettelStore Z.ZettelGraph a -> (Z.ZettelStore, Z.ZettelGraph, a) -> Action ()) ->
  [FilePath] ->
  Action (Z.ZettelStore, Z.ZettelGraph)
generateSite config writeHtmlRoute' zettelsPat = do
  when (olderThan $ Z.minVersion config)
    $ fail
    $ toString
    $ "Require neuron mininum version " <> Z.minVersion config <> ", but your neuron version is " <> neuronVersion
  zettelStore <- Z.mkZettelStore =<< Rib.forEvery zettelsPat pure
  zettelGraph <- either (fail . toString) pure $ Z.mkZettelGraph zettelStore
  let writeHtmlRoute v r = writeHtmlRoute' r (zettelStore, zettelGraph, v)
  -- Generate HTML for every zettel
  forM_ (Map.toList zettelStore) $ \(k, v) ->
    -- TODO: Should `Zettel` not contain ZettelID?
    -- See duplication in `renderZettel`
    writeHtmlRoute v $ Z.Route_Zettel k
  -- Generate the z-index
  writeHtmlRoute () Z.Route_ZIndex
  -- Generate search page
  writeHtmlRoute () Z.Route_Search
  -- Write alias redirects, unless a zettel with that name exists.
  aliases <- Z.getAliases config zettelStore
  forM_ aliases $ \Z.Alias {..} ->
    writeHtmlRoute targetZettel (Z.Route_Redirect aliasZettel)
  pure (zettelStore, zettelGraph)
