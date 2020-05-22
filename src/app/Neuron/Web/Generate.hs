{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Main module for using neuron as a library, instead of as a CLI tool.
module Neuron.Web.Generate
  ( generateSite,
    loadZettels,
  )
where

import Control.Monad.Except (MonadError, liftEither, runExceptT, withExceptT)
import qualified Data.Graph.Labelled as G
import Data.Traversable
import Development.Shake
import Neuron.Config (Config (..))
import Neuron.Config.Alias (Alias (..), getAliases)
import Neuron.Version (neuronVersion, olderThan)
import qualified Neuron.Web.Route as Z
import Neuron.Zettelkasten.Connection (Connection (..))
import Neuron.Zettelkasten.Error (NeuronError (..))
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.ID (mkZettelID)
import Neuron.Zettelkasten.Query.Eval (queryConnections)
import Neuron.Zettelkasten.Zettel (Zettel, ZettelT (..), mkZettelFromMarkdown)
import Options.Applicative
import Relude
import qualified Rib
import System.FilePath

-- | Generate the Zettelkasten site
generateSite ::
  Config ->
  (forall a. Z.Route ZettelGraph a -> (ZettelGraph, a) -> Action ()) ->
  Action ZettelGraph
generateSite config writeHtmlRoute' = do
  when (olderThan $ minVersion config)
    $ fail
    $ toString
    $ "Require neuron mininum version " <> minVersion config <> ", but your neuron version is " <> neuronVersion
  zettelGraph <- loadZettelkasten
  let writeHtmlRoute v r = writeHtmlRoute' r (zettelGraph, v)
  -- Generate HTML for every zettel
  forM_ (G.getZettels zettelGraph) $ \z ->
    -- TODO: Should `Zettel` not contain ZettelID?
    -- See duplication in `renderZettel`
    writeHtmlRoute z $ Z.Route_Zettel (zettelID z)
  -- Generate the z-index
  writeHtmlRoute () Z.Route_ZIndex
  -- Generate search page
  writeHtmlRoute () Z.Route_Search
  -- Write alias redirects, unless a zettel with that name exists.
  aliases <- getAliases config zettelGraph
  forM_ aliases $ \Alias {..} ->
    writeHtmlRoute targetZettel (Z.Route_Redirect aliasZettel)
  pure zettelGraph

loadZettels :: Action [Zettel]
loadZettels =
  fmap G.getZettels loadZettelkasten

loadZettelkasten :: Action ZettelGraph
loadZettelkasten =
  loadZettelkastenFrom =<< Rib.forEvery ["*.md"] pure

-- | Load the Zettelkasten from disk, using the given list of zettel files
loadZettelkastenFrom :: [FilePath] -> Action ZettelGraph
loadZettelkastenFrom files = do
  notesDir <- Rib.ribInputDir
  zettels <- forM files $ \((notesDir </>) -> path) -> do
    s <- toText <$> readFile' path
    let zid = mkZettelID path
    case mkZettelFromMarkdown zid s snd of
      Left e -> fail $ toString e
      Right zettel -> pure zettel
  either (fail . show) pure $ mkZettelGraph zettels

-- | Build the Zettelkasten graph from a list of zettels
--
-- Also return the markdown extension to use for each zettel.
mkZettelGraph ::
  forall m.
  MonadError NeuronError m =>
  [Zettel] ->
  m ZettelGraph
mkZettelGraph zettels = do
  res :: [(Zettel, [(Maybe Connection, Zettel)])] <- liftEither =<< do
    flip runReaderT zettels $ runExceptT $ do
      -- TODO: re: Left; for Right, we must render the query, which only happens
      -- in Web.View. How do we accumulate the errors?
      for zettels $ \z -> fmap (z,) $ do
        withExceptT (NeuronError_BadQuery (zettelID z) . Left) $ do
          queryConnections (zettelContent z)
  let g :: ZettelGraph = G.mkGraphFrom (fst <$> res) $ flip concatMap res $ \(z1, conns) ->
        conns <&> \(c, z2) -> (connectionMonoid (fromMaybe Folgezettel c), z1, z2)
  pure g
  where
    connectionMonoid = Just
