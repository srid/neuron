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

import Development.Shake (Action)
import qualified Neuron.Config as Z
import Neuron.Version (neuronVersion, olderThan)
import qualified Neuron.Web.Route as Z
import qualified Neuron.Zettelkasten.Graph as G
import qualified Neuron.Zettelkasten.Graph.Type as G
import qualified Neuron.Zettelkasten.Zettel as Z
import Options.Applicative
import Relude
import qualified Rib

-- | Generate the Zettelkasten site
generateSite ::
  Z.Config ->
  (forall a. Z.Route G.ZettelGraph a -> (G.ZettelGraph, a) -> Action ()) ->
  [FilePath] ->
  Action G.ZettelGraph
generateSite config writeHtmlRoute' zettelsPat = do
  when (olderThan $ Z.minVersion config)
    $ fail
    $ toString
    $ "Require neuron mininum version " <> Z.minVersion config <> ", but your neuron version is " <> neuronVersion
  zettelGraph <- G.loadZettelkasten =<< Rib.forEvery zettelsPat pure
  let writeHtmlRoute v r = writeHtmlRoute' r (zettelGraph, v)
      zettels = G.getVertices zettelGraph
  -- Generate HTML for every zettel
  forM_ zettels $ \z ->
    -- TODO: Should `Zettel` not contain ZettelID?
    -- See duplication in `renderZettel`
    writeHtmlRoute z $ Z.Route_Zettel (Z.zettelID z)
  -- Generate the z-index
  writeHtmlRoute () Z.Route_ZIndex
  -- Generate search page
  writeHtmlRoute () Z.Route_Search
  -- Write alias redirects, unless a zettel with that name exists.
  aliases <- Z.getAliases config zettelGraph
  forM_ aliases $ \Z.Alias {..} ->
    writeHtmlRoute targetZettel (Z.Route_Redirect aliasZettel)
  pure zettelGraph
