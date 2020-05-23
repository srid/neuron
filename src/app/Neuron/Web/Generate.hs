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
    loadZettelsIgnoringErrors,
  )
where

import Control.Monad.Writer (runWriterT)
import qualified Data.Graph.Labelled as G
import qualified Data.Map.Strict as Map
import Data.Traversable
import Development.Shake
import Neuron.Config (Config (..))
import Neuron.Config.Alias (Alias (..), getAliases)
import Neuron.Version (neuronVersion, olderThan)
import qualified Neuron.Web.Route as Z
import Neuron.Zettelkasten.Connection (Connection (..))
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.ID (ZettelID, mkZettelID)
import Neuron.Zettelkasten.Query.Error (QueryParseError)
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
  -- NOTE: We ignore errors, because they will be displayed during rendering.
  (zettelGraph, _errors) <- loadZettelkasten
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

loadZettelsIgnoringErrors :: Action [Zettel]
loadZettelsIgnoringErrors =
  fmap (G.getZettels . fst) loadZettelkasten

loadZettelkasten :: Action (ZettelGraph, Map ZettelID [QueryParseError])
loadZettelkasten =
  loadZettelkastenFrom =<< Rib.forEvery ["*.md"] pure

-- | Load the Zettelkasten from disk, using the given list of zettel files
loadZettelkastenFrom :: HasCallStack => [FilePath] -> Action (ZettelGraph, Map ZettelID [QueryParseError])
loadZettelkastenFrom files = do
  notesDir <- Rib.ribInputDir
  zettels <- forM files $ \((notesDir </>) -> path) -> do
    s <- toText <$> readFile' path
    let zid = mkZettelID path
    case mkZettelFromMarkdown zid s snd of
      Left e -> fail $ toString e
      Right zettel -> pure zettel
  mkZettelGraph zettels

-- | Build the Zettelkasten graph from a list of zettels
--
-- Also return the markdown extension to use for each zettel.
mkZettelGraph ::
  forall m.
  Monad m =>
  [Zettel] ->
  m (ZettelGraph, Map ZettelID [QueryParseError])
mkZettelGraph zettels = do
  res :: [(Zettel, ([(Maybe Connection, Zettel)], [QueryParseError]))] <- do
    flip runReaderT zettels $ do
      for zettels $ \z -> fmap (z,) $ do
        runWriterT $ queryConnections (zettelContent z)
  let g :: ZettelGraph = G.mkGraphFrom (fst <$> res) $ flip concatMap res $ \(z1, fst -> conns) ->
        conns <&> \(c, z2) -> (connectionMonoid (fromMaybe Folgezettel c), z1, z2)
  pure
    ( g,
      Map.fromListWith (++) $ flip fmap res $ \(z, (_conns, errs)) ->
        (zettelID z, errs)
    )
  where
    connectionMonoid = Just
