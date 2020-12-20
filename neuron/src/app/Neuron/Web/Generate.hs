{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Main module for using neuron as a library, instead of as a CLI tool.
module Neuron.Web.Generate
  ( generateSite,
    loadZettelkasten,
    loadZettelkastenGraph,
  )
where

import Data.FileEmbed (embedOneStringFileOf)
import qualified Data.Map.Strict as Map
import Data.Tagged (untag)
import qualified Data.Text as T
import Data.Traversable (for)
import Development.Shake (Action, need)
import Neuron.Config.Type (Config)
import qualified Neuron.Config.Type as C
import Neuron.Reader (readerForZettelFormat)
import Neuron.Reader.Type (ZettelFormat, zettelFormatToExtension)
import Neuron.Version (neuronVersion, olderThan)
import qualified Neuron.Web.Cache as Cache
import Neuron.Web.Cache.Type (NeuronCache, _neuronCache_graph)
import qualified Neuron.Web.Cache.Type as Cache
import Neuron.Web.Generate.Route ()
import qualified Neuron.Web.Route as Z
import qualified Neuron.Zettelkasten.Graph.Build as G
import Neuron.Zettelkasten.Graph.Type (ZettelGraph, stripSurroundingContext)
import Neuron.Zettelkasten.ID (ZettelID, getZettelID, unZettelID)
import Neuron.Zettelkasten.Query.Error (showQueryResultError)
import Neuron.Zettelkasten.Zettel
  ( ZettelC,
    ZettelError (..),
    ZettelT (zettelSlug),
    sansContent,
  )
import Neuron.Zettelkasten.Zettel.Parser (extractQueriesWithContext, parseZettels)
import Relude
import Rib.Shake (forEvery, ribInputDir)
import System.FilePath ((</>))

-- | The contents of GHCJS compiled JS.
--
-- We specify an alternate path, that is relative to project root, so that
-- ghcide will be able to compile this module.
impulseJS :: Text
impulseJS = $(embedOneStringFileOf ["./ghcjs/impulse.js", "./neuron/ghcjs/impulse.js"])

-- | Generate the Zettelkasten site
generateSite ::
  Config ->
  (forall a. Z.Route a -> (ZettelGraph, a) -> Action ()) ->
  Action ZettelGraph
generateSite config writeHtmlRoute' = do
  when (olderThan $ C.minVersion config) $ do
    fail $
      toString $
        "Require neuron mininum version "
          <> C.minVersion config
          <> ", but your neuron version is "
          <> neuronVersion
  (Cache.NeuronCache {..}, zettelContents) <- loadZettelkasten config
  let writeHtmlRoute :: forall a. a -> Z.Route a -> Action ()
      writeHtmlRoute v r = writeHtmlRoute' r (_neuronCache_graph, v)
  -- Generate HTML for every zettel
  forM_ zettelContents $ \val@(sansContent -> z) ->
    writeHtmlRoute val $ Z.Route_Zettel (zettelSlug z)
  -- Generate search page
  writeHtmlRoute impulseJS $ Z.Route_Impulse Nothing
  -- Report all errors
  forM_ (Map.toList _neuronCache_errors) $ \(zid, errs) -> do
    for errs $ \err -> reportError zid $
      case err of
        ZettelError_ParseError (untag . snd -> parseErr) ->
          parseErr :| []
        ZettelError_QueryResultErrors queryErrs ->
          showQueryResultError <$> snd queryErrs
        ZettelError_AmbiguousID filePaths ->
          ("Multiple zettels have the same ID: " <> T.intercalate ", " (toText <$> toList filePaths))
            :| []
        ZettelError_AmbiguousSlug slug ->
          "Slug '" <> slug <> "' is already used by another zettel" :| []
  pure _neuronCache_graph

-- | Report an error in the terminal
reportError :: MonadIO m => ZettelID -> NonEmpty Text -> m ()
reportError zid errors = do
  putTextLn $ "E " <> unZettelID zid
  forM_ errors $ \err ->
    putText $ "  - " <> indentAllButFirstLine 4 err
  where
    indentAllButFirstLine :: Int -> Text -> Text
    indentAllButFirstLine n = unlines . go . lines
      where
        go [] = []
        go [x] = [x]
        go (x : xs) =
          x : fmap (toText . (replicate n ' ' <>) . toString) xs

-- | Like `loadZettelkasten` but without the content
--
-- Also allows retrieving the cached data for faster execution.
loadZettelkastenGraph ::
  Config ->
  Action NeuronCache
loadZettelkastenGraph =
  fmap fst . loadZettelkasten

loadZettelkasten :: Config -> Action (NeuronCache, [ZettelC])
loadZettelkasten config = do
  formats <- C.getZettelFormats config
  -- Experimental feature; see https://github.com/srid/neuron/issues/309
  let patternPrefix = bool "*" "**/*" $ C.recurseDir config
  zettelFiles <- forM formats $ \fmt -> do
    let pat = toString $ patternPrefix <> zettelFormatToExtension fmt
    files <- forEvery [pat] pure
    pure (fmt, files)
  (g, zs, errs) <- loadZettelkastenFrom zettelFiles
  let cache = Cache.NeuronCache g errs config neuronVersion
      cacheSmall = cache {_neuronCache_graph = stripSurroundingContext g}
  Cache.updateCache cacheSmall
  pure (cache, zs)

-- | Load the Zettelkasten from disk, using the given list of zettel files
loadZettelkastenFrom ::
  NonEmpty (ZettelFormat, [FilePath]) ->
  Action
    ( ZettelGraph,
      [ZettelC],
      Map ZettelID (NonEmpty ZettelError)
    )
loadZettelkastenFrom fs = do
  notesDir <- ribInputDir
  -- Use State monad to "gather" duplicate zettel files using same IDs, in the
  -- `Right` of the Either state value; with the `Left` collecting the actual
  -- zettel files to load into the graph.
  zidMap :: Map ZettelID (Either (ZettelFormat, (FilePath, Text)) (NonEmpty FilePath)) <-
    fmap snd $
      flip runStateT Map.empty $ do
        forM_ fs $ \(format, files) -> do
          forM_ files $ \relPath -> do
            case getZettelID format relPath of
              Nothing ->
                pure ()
              Just zid -> do
                get
                  >>= ( \case
                          Just (Left (_f, (oldPath, _s))) -> do
                            -- The zettel ID is already used by `oldPath`. Mark it as a dup.
                            modify $ Map.insert zid (Right $ relPath :| [oldPath])
                          Just (Right (toList -> ambiguities)) -> do
                            -- Third or later duplicate file with the same Zettel ID
                            modify $ Map.insert zid (Right $ relPath :| ambiguities)
                          Nothing -> do
                            let absPath = notesDir </> relPath
                            lift $ need [absPath]
                            s <- decodeUtf8With lenientDecode <$> readFileBS absPath
                            modify $ Map.insert zid (Left (format, (relPath, s)))
                      )
                    . Map.lookup zid
  let dups = ZettelError_AmbiguousID <$> Map.mapMaybe rightToMaybe zidMap
      files =
        fmap (first (id &&& readerForZettelFormat)) $
          Map.toList $
            Map.fromListWith (<>) $
              flip fmap (Map.toList $ Map.mapMaybe leftToMaybe zidMap) $
                \(zid, (fmt, (path, s))) ->
                  (fmt, [(zid, path, s)])
      zs = parseZettels extractQueriesWithContext files
      (g, gerrs) = G.buildZettelkasten zs
      errs = Map.unionsWith (<>) [one <$> dups, one <$> gerrs]
  pure (g, zs, errs)
