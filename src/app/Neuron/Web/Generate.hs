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
    loadZettelkasten,
    loadZettelsIgnoringErrors,
  )
where

import qualified Data.Map.Strict as Map
import Data.Traversable
import Development.Shake
import Neuron.Config (Config (..))
import Neuron.Config.Alias (Alias (..), getAliases)
import Neuron.Version (neuronVersion, olderThan)
import qualified Neuron.Web.Route as Z
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.ID (ZettelID, mkZettelID)
import Neuron.Zettelkasten.Query.Error (QueryParseError, showQueryError)
import Neuron.Zettelkasten.Zettel
import Options.Applicative
import Reflex.Class (filterLeft, filterRight)
import Relude
import qualified Rib
import Rib.Route
import System.FilePath

-- | Generate the Zettelkasten site
generateSite ::
  Config ->
  (forall a. Z.Route ZettelGraph a -> (ZettelGraph, a) -> Action (Z.RouteError a)) ->
  Action ZettelGraph
generateSite config writeHtmlRoute' = do
  when (olderThan $ minVersion config)
    $ fail
    $ toString
    $ "Require neuron mininum version " <> minVersion config <> ", but your neuron version is " <> neuronVersion
  (zettelGraph, zettelContents, errors) <- loadZettelkasten
  -- NOTE: Right errors are handled further below in individual zettel generation.
  let skippedErrors = Map.mapMaybe leftToMaybe errors
      writeHtmlRoute :: forall a. a -> Z.Route ZettelGraph a -> Action (Z.RouteError a)
      writeHtmlRoute v r = writeHtmlRoute' r (zettelGraph, v)
  -- Generate HTML for every zettel
  forM_ zettelContents $ \val@(PandocZettel (z, _)) -> do
    let r = Z.Route_Zettel $ zettelID z
    zerrors <- writeHtmlRoute val r
    unless (null zerrors) $ do
      reportError r Nothing $ showQueryError <$> zerrors
  -- Generate the z-index
  writeHtmlRoute errors Z.Route_ZIndex
  -- Generate search page
  writeHtmlRoute () Z.Route_Search
  -- Write alias redirects, unless a zettel with that name exists.
  aliases <- getAliases config zettelGraph
  forM_ aliases $ \Alias {..} ->
    writeHtmlRoute targetZettel (Z.Route_Redirect aliasZettel)
  forM_ (Map.toList skippedErrors) $ \(zid, err) -> do
    reportError (Z.Route_Zettel zid) (Just "SKIPPED") [err]
  pure zettelGraph

-- | Report an error in the terminal
reportError :: (MonadIO m, IsRoute r) => r a -> Maybe Text -> [Text] -> m ()
reportError route mErrorKind errors = do
  putTextLn $ "E " <> fromMaybe "Unknown route" (fmap toText $ routeFile route) <> maybe "" (\x -> " (" <> x <> ")") mErrorKind
  forM_ errors $ \err ->
    putText $ "  - " <> indentAllButFirstLine 4 err

indentAllButFirstLine :: Int -> Text -> Text
indentAllButFirstLine n = unlines . go . lines
  where
    go [] = []
    go [x] = [x]
    go (x : xs) =
      x : fmap (toText . (take n (repeat ' ') <>) . toString) xs

loadZettelsIgnoringErrors :: Action [Zettel]
loadZettelsIgnoringErrors = do
  (g, _, _) <- loadZettelkasten
  pure $ G.getZettels g

loadZettelkasten ::
  Action
    ( ZettelGraph,
      [PandocZettel],
      Map ZettelID (Either Text [QueryParseError])
    )
loadZettelkasten =
  loadZettelkastenFrom =<< Rib.forEvery ["*.md"] pure

-- | Load the Zettelkasten from disk, using the given list of zettel files
loadZettelkastenFrom ::
  [FilePath] ->
  Action
    ( ZettelGraph,
      [PandocZettel],
      Map ZettelID (Either Text [QueryParseError])
    )
loadZettelkastenFrom files = do
  notesDir <- Rib.ribInputDir
  parseRes <- forM files $ \((notesDir </>) -> path) -> do
    s <- toText <$> readFile' path
    let zid = mkZettelID path
    pure $ first (zid,) $ parseZettel zid s
  let skippedZettelErrors :: [(ZettelID, Text)] = filterLeft parseRes
      zs = filterRight parseRes
      (g, errors) = G.mkZettelGraph zs
  pure (g, zs, fmap Left (Map.fromList skippedZettelErrors) `Map.union` fmap Right errors)
