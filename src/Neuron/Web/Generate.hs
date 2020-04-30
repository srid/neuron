{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import qualified Neuron.Zettelkasten.Zettel as Z
import Options.Applicative
import Relude

-- | Generate the Zettelkasten site
generateSite ::
  Z.Config ->
  (forall a. Z.Route G.ZettelGraph a -> (G.ZettelGraph, a) -> Action ()) ->
  Action G.ZettelGraph
generateSite config writeHtmlRoute' = do
  when (olderThan $ Z.minVersion config)
    $ fail
    $ toString
    $ "Require neuron mininum version " <> Z.minVersion config <> ", but your neuron version is " <> neuronVersion
  (zettelGraph, zettels) <- G.loadZettelkasten
  let writeHtmlRoute v r = writeHtmlRoute' r (zettelGraph, v)
  -- Generate HTML for every zettel
  forM_ zettels $ \(z, d) ->
    -- TODO: Should `Zettel` not contain ZettelID?
    -- See duplication in `renderZettel`
    writeHtmlRoute (z, d) $ Z.Route_Zettel (Z.zettelID z)
  -- Generate the z-index
  writeHtmlRoute () Z.Route_ZIndex
  -- Generate search page
  writeHtmlRoute () Z.Route_Search
  -- Write alias redirects, unless a zettel with that name exists.
  aliases <- Z.getAliases config zettelGraph
  forM_ aliases $ \Z.Alias {..} ->
    writeHtmlRoute targetZettel (Z.Route_Redirect aliasZettel)
  pure zettelGraph
