{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Main module for using neuron as a library, instead of as a CLI tool.
module Neuron.Web.Generate
  ( generateSite,
    loadZettelkasten,
  )
where

import Control.Monad.Writer.Strict (runWriter, tell)
import qualified Data.List
import qualified Data.Map.Strict as Map
import Development.Shake (Action)
import Neuron.Config.Type (Config)
import qualified Neuron.Plugin.DirectoryFolgezettel as DF
import Neuron.Plugin.Type (Plugin (..))
import Neuron.Version (neuronVersion)
import qualified Neuron.Web.Cache as Cache
import Neuron.Web.Cache.Type (NeuronCache, _neuronCache_graph)
import qualified Neuron.Web.Cache.Type as Cache
import Neuron.Web.Generate.Route ()
import qualified Neuron.Web.Route as Z
import qualified Neuron.Zettelkasten.Graph.Build as G
import Neuron.Zettelkasten.Graph.Type (ZettelGraph, stripSurroundingContext)
import Neuron.Zettelkasten.ID (ZettelID (..))
import qualified Neuron.Zettelkasten.Resolver as R
import Neuron.Zettelkasten.Zettel
  ( ZettelC,
    ZettelT (zettelSlug),
    sansContent,
  )
import Neuron.Zettelkasten.Zettel.Error
  ( ZettelError (ZettelError_AmbiguousID),
    zettelErrorList,
  )
import Neuron.Zettelkasten.Zettel.Parser (extractQueriesWithContext, parseZettels)
import Reflex (ffor)
import Relude hiding (traceShowId)
import Rib.Shake (ribInputDir)
import System.Directory (withCurrentDirectory)
import qualified System.Directory.Contents as DC
import System.FilePath (takeExtension)

-- | Enabled neuron plugins
-- TODO: allow it to be specified in neuron.dhall
plugins :: [Plugin]
plugins =
  [ DF.plugin
  ]

-- | Generate the Zettelkasten site
generateSite ::
  Config ->
  (forall a. NeuronCache -> Z.Route a -> a -> Action ()) ->
  Action ZettelGraph
generateSite config writeHtmlRoute' = do
  (cache@Cache.NeuronCache {..}, zettelContents) <- loadZettelkasten config
  let writeHtmlRoute :: forall a. a -> Z.Route a -> Action ()
      writeHtmlRoute v r = writeHtmlRoute' cache r v
  -- Generate HTML for every zettel
  forM_ zettelContents $ \val@(zettelSlug . sansContent -> slug) ->
    writeHtmlRoute val $ Z.Route_Zettel slug
  -- Generate search page
  writeHtmlRoute () $ Z.Route_Impulse Nothing
  -- Report all errors
  -- TODO: Report only new errors in this run, to avoid spamming the terminal.
  forM_ (Map.toList _neuronCache_errors) $ \(zid, err) -> do
    reportError zid $ zettelErrorList err
  pure _neuronCache_graph
  where
    -- Report an error in the terminal
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

loadZettelkasten :: Config -> Action (NeuronCache, [ZettelC])
loadZettelkasten config = do
  ((g, zs), errs) <-
    loadZettelkastenFromFiles =<< locateZettelFiles
  let cache = Cache.NeuronCache g errs config neuronVersion
      cacheSmall = cache {_neuronCache_graph = stripSurroundingContext g}
  Cache.updateCache cacheSmall
  pure (cache, zs)
  where
    -- TODO:
    -- If making recurseDir the default,
    -- - [ ] Allow blacklist (eg: not "README.md"; default being ".*"; always ignore .neuron)
    locateZettelFiles = do
      -- Run with notes dir as PWD, so that DirTree uses relative paths throughout.
      inNotesDir $
        DC.buildDirTree "." >>= \case
          Just t@(DC.DirTree_Dir _notesDir' _toplevel) -> do
            let mt' = DC.pruneDirTree =<< DC.filterDirTree includeDirEntry t
            maybe (fail "No markdown files?") pure mt'
          _ -> fail "Directory error?"
    includeDirEntry name =
      Just True
        == ( do
               guard $ not $ isDotfile name
               guard $ takeExtension name == ".md"
               pure True
           )
    isDotfile name =
      "." `Data.List.isPrefixOf` name
        && not ("./" `Data.List.isPrefixOf` name)
    inNotesDir :: IO b -> Action b
    inNotesDir a = do
      d <- ribInputDir
      liftIO $ withCurrentDirectory d a

-- | Load the Zettelkasten from disk, using the given list of zettel files
loadZettelkastenFromFiles ::
  DC.DirTree FilePath ->
  Action
    ( ( ZettelGraph,
        [ZettelC]
      ),
      Map ZettelID ZettelError
    )
loadZettelkastenFromFiles fileTree = do
  let total = getSum @Int $ foldMap (const $ Sum 1) fileTree
  -- TODO: Would be nice to show a progressbar here
  liftIO $ putStrLn $ "Loading directory tree (" <> show total <> " files) ..."
  zidRefs <-
    fmap snd $
      flip runStateT Map.empty $ do
        R.resolveZidRefsFromDirTree fileTree
        forM_ (_plugin_afterZettelRead <$> plugins) $ \f -> f fileTree
  pure $
    runWriter $ do
      filesWithContent <-
        flip Map.traverseMaybeWithKey zidRefs $ \zid -> \case
          R.ZIDRef_Ambiguous fps -> do
            tell $ one (zid, ZettelError_AmbiguousID fps)
            pure Nothing
          R.ZIDRef_Available fp s ->
            pure $ Just (fp, s)
      let zs =
            ffor (parseZettels extractQueriesWithContext $ Map.toList filesWithContent) $ \z ->
              foldl' (\z1 f -> f z1) z (_plugin_afterZettelParse <$> plugins)
      g <- G.buildZettelkasten zs
      pure (g, zs)
