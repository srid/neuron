{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | React to the world, and build our Zettelkasten
-- TODO: Split this module appropriately.
module Neuron.Reactor where

import Colog
import Control.Monad.Fix (MonadFix)
import Control.Monad.Writer.Strict (runWriter, tell)
import Data.List (nubBy)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time (NominalDiffTime)
import Neuron.CLI.Types
  ( App,
    MonadApp (..),
    getAppEnv,
    getNotesDir,
    runApp,
  )
import qualified Neuron.Cache as Cache
import Neuron.Cache.Type (NeuronCache)
import qualified Neuron.Cache.Type as Cache
import Neuron.Config (getConfig)
import Neuron.Config.Type (Config)
import qualified Neuron.Config.Type as Config
import Neuron.Frontend.Manifest (Manifest)
import qualified Neuron.Frontend.Manifest as Manifest
import Neuron.Frontend.Route (Route (Route_Impulse), routeHtmlPath)
import qualified Neuron.Frontend.Route as Z
import qualified Neuron.Frontend.Route.Data as RD
import qualified Neuron.Frontend.Static.HeadHtml as HeadHtml
import qualified Neuron.Frontend.Static.Html as Html
import qualified Neuron.Frontend.Widget as W
import Neuron.Plugin (PluginRegistry)
import qualified Neuron.Plugin as Plugin
import Neuron.Version (neuronVersion)
import qualified Neuron.Zettelkasten.Graph.Build as G
import Neuron.Zettelkasten.Graph.Type (ZettelGraph, stripSurroundingContext)
import Neuron.Zettelkasten.ID (ZettelID (..), zettelIDSourceFileName)
import qualified Neuron.Zettelkasten.Resolver as R
import Neuron.Zettelkasten.Zettel
  ( ZettelC,
  )
import Neuron.Zettelkasten.Zettel.Error
  ( ZettelError (..),
    ZettelIssue (..),
    zettelErrorText,
  )
import Reflex
import Reflex.Dom.Core
  ( HydratableT (runHydratableT),
    renderStatic,
  )
import Reflex.FSNotify (FSEvent, watchTree)
import Reflex.Host.Headless (runHeadlessApp)
import Relude
import System.Directory (doesFileExist, withCurrentDirectory)
import qualified System.Directory.Contents as DC
import qualified System.Directory.Contents.Extra as DC
import qualified System.FSNotify as FSN
import System.FilePath (takeExtension, (</>))

generateSite :: Bool -> App ()
generateSite continueMonitoring = do
  -- Initial gen
  doGen
  -- Continue morning
  when continueMonitoring $ do
    log D "Finished generating; monitoring for changes."
    appEnv <- getAppEnv
    notesDir <- getNotesDir
    liftIO $
      runHeadlessApp $ do
        fsChanged <- watchDirWithDebounce 0.1 notesDir
        performEvent_ $
          ffor fsChanged $ \(fmap FSN.eventPath -> paths) -> do
            liftIO $
              runApp appEnv $ do
                baseDir <- getNotesDir
                forM_ paths $ \(DC.mkRelative baseDir -> path) ->
                  log I $ toText $ "M " <> path
                doGen
                log D "Finished generating; monitoring for changes."
        pure never
  where
    -- Do a one-off generation from top to bottom.
    -- No incrementall stuff (yet)
    doGen :: App ()
    doGen = do
      (cache, RD.mkRouteDataCache -> rdCache, fileTree) <- loadZettelkasten =<< getConfig
      -- Static files
      case DC.walkContents "static" fileTree of
        Just staticTree@(DC.DirTree_Dir _ _) -> do
          notesDir <- getNotesDir
          outputDir <- getOutputDir
          DC.rsyncDir notesDir outputDir staticTree
        _ ->
          pure ()
      headHtml <- HeadHtml.getHeadHtmlFromTree fileTree
      let manifest = Manifest.mkManifestFromTree fileTree
          slugs = RD.allSlugs rdCache
      -- +2 for Impulse routes
      log D $ "Rendering routes (" <> show (length slugs + 2) <> " slugs) ..."
      -- Do it
      let wH :: Route a -> App ()
          wH r = writeRouteHtml r =<< genRouteHtml headHtml manifest cache rdCache r
      (wH . Z.Route_Zettel) `mapM_` slugs
      wH $ Z.Route_Impulse Nothing
      wH Z.Route_ImpulseStatic
      reportAllErrors cache
      pure ()

-- Report all errors
-- TODO: Report only new errors in this run, to avoid spamming the terminal.
reportAllErrors :: forall m env. (MonadIO m, WithLog env Message m) => NeuronCache -> m ()
reportAllErrors cache = do
  missingLinks <- fmap sum $
    forM (Map.toList $ Cache._neuronCache_errors cache) $ \(zid, issue) -> do
      case issue of
        ZettelIssue_MissingLinks (_slug, qErrs) ->
          pure (length qErrs)
        ZettelIssue_Error e -> do
          reportError zid e
          pure 0
  when (missingLinks > 0) $
    log W $ show missingLinks <> " missing links found across zettels (see Impulse)"
  where
    -- Report an error in the terminal
    reportError :: ZettelID -> ZettelError -> m ()
    reportError zid (zettelErrorText -> err) = do
      log E $ toText $ zettelIDSourceFileName zid
      log E $ "  - " <> indentAllButFirstLine 4 err
      where
        indentAllButFirstLine :: Int -> Text -> Text
        indentAllButFirstLine n = unlines . go . lines
          where
            go [] = []
            go [x] = [x]
            go (x : xs) =
              x : fmap (toText . (replicate n ' ' <>) . toString) xs

genRouteHtml ::
  forall m a.
  MonadIO m =>
  HeadHtml.HeadHtml ->
  Manifest ->
  NeuronCache ->
  RD.RouteDataCache ->
  Route a ->
  m ByteString
genRouteHtml headHtml manifest cache rdCache r = do
  -- We do this verbose dance to make sure hydration happens only on Impulse route.
  -- Ideally, this should be abstracted out, but polymorphic types are a bitch.
  liftIO $ case r of
    Route_Impulse {} ->
      fmap snd . renderStatic . runHydratableT $ do
        -- FIXME: Injecting initial value here will break hydration on Impulse.
        let cacheDyn = constDyn $ W.LoadableData Nothing
        Html.renderRoutePage cacheDyn rdCache headHtml manifest r
    _ ->
      fmap snd . renderStatic $ do
        let cacheDyn = constDyn $ W.availableData cache
        Html.renderRoutePage cacheDyn rdCache headHtml manifest r

writeRouteHtml :: (MonadApp m, MonadIO m, WithLog env Message m) => Route a -> ByteString -> m ()
writeRouteHtml r content = do
  outputDir <- getOutputDir
  let htmlFile = outputDir </> routeHtmlPath r
  -- DOCTYPE declaration is helpful for code that might appear in the user's `head.html` file (e.g. KaTeX).
  let s = decodeUtf8 @Text $ "<!DOCTYPE html>" <> content
  s0 <- liftIO $ do
    doesFileExist htmlFile >>= \case
      True -> Just <$> readFileText htmlFile
      False -> pure Nothing
  unless (Just s == s0) $ do
    log I $ toText $ "+ " <> DC.mkRelative outputDir htmlFile
    writeFileText htmlFile s

-- | Like `watchDir` but batches file events
--
-- Returned event is in the form of list, which is guaranteed to not have repeat
-- events (i.e., 2 or more events with the same eventPath)
watchDirWithDebounce ::
  ( PostBuild t m,
    TriggerEvent t m,
    PerformEvent t m,
    MonadIO (Performable m),
    MonadHold t m,
    MonadFix m
  ) =>
  NominalDiffTime ->
  FilePath ->
  m (Event t [FSEvent])
watchDirWithDebounce ms dirPath = do
  let cfg = FSN.defaultConfig {FSN.confDebounce = FSN.Debounce ms}
  pb <- getPostBuild
  evt <- watchTree cfg (dirPath <$ pb) (const True)
  -- TODO: support with .neuronignore
  let evt2 = flip ffilter evt $ \(toText . FSN.eventPath -> path) ->
        not (".neuron/" `T.isInfixOf` path || ".git" `T.isInfixOf` path)
  evtGrouped <- fmap toList <$> batchOccurrences ms evt2
  -- Discard all but the last event for each path.
  pure $ nubByKeepLast ((==) `on` FSN.eventPath) <$> evtGrouped
  where
    -- Like @Data.List.nubBy@ but keeps the last occurence
    nubByKeepLast :: (a -> a -> Bool) -> [a] -> [a]
    nubByKeepLast f =
      reverse . nubBy f . reverse

-- Functions from old Generate.hs

loadZettelkasten ::
  (MonadIO m, MonadApp m, MonadFail m, WithLog env Message m) =>
  Config ->
  m (NeuronCache, [ZettelC], DC.DirTree FilePath)
loadZettelkasten config = do
  let plugins = Config.getPlugins config
  -- TODO Instead of logging here, put this info in Impulse footer.
  log D $ "Plugins enabled: " <> Plugin.pluginRegistryShow plugins
  fileTree <- locateZettelFiles plugins
  ((g, zs), errs) <- loadZettelkastenFromFiles plugins fileTree
  let cache = Cache.NeuronCache g errs config neuronVersion
      cacheSmall = cache {Cache._neuronCache_graph = stripSurroundingContext g}
  Cache.updateCache cacheSmall
  pure (cache, zs, fileTree)

locateZettelFiles :: (MonadIO m, MonadApp m) => PluginRegistry -> m (DC.DirTree FilePath)
locateZettelFiles plugins = do
  -- Run with notes dir as PWD, so that DirTree uses relative paths throughout.
  d <- getNotesDir
  liftIO $
    withCurrentDirectory d $
      -- Building with "." as root directory ensures that the returned tree
      -- structure consistently uses the "./" prefix for all paths. This
      -- assumption then holds elsewhere in neuron.
      DC.buildDirTree "." >>= \case
        Just t -> do
          Plugin.filterSources plugins t >>= \case
            Nothing ->
              fail "No source files to process"
            Just tF ->
              pure tF
        Nothing ->
          fail "No sources"

-- | Load the Zettelkasten from disk, using the given list of zettel files
loadZettelkastenFromFiles ::
  (MonadIO m, MonadApp m, MonadFail m, WithLog env Message m) =>
  PluginRegistry ->
  DC.DirTree FilePath ->
  m
    ( ( ZettelGraph,
        [ZettelC]
      ),
      Map ZettelID ZettelIssue
    )
loadZettelkastenFromFiles plugins fileTree = do
  zidRefs <-
    case DC.pruneDirTree =<< DC.filterDirTree ((== ".md") . takeExtension) fileTree of
      Nothing ->
        pure mempty
      Just mdFileTree -> do
        let total = getSum @Int $ foldMap (const $ Sum 1) mdFileTree
        -- TODO: Would be nice to show a progressbar here
        -- liftIO $ DC.printDirTree fileTree
        log D $ "Loading directory tree (" <> show total <> " .md files) ..."
        fmap snd $
          flip runStateT Map.empty $ do
            flip R.resolveZidRefsFromDirTree mdFileTree $ \relPath -> do
              absPath <- fmap (</> relPath) getNotesDir
              decodeUtf8With lenientDecode <$> readFileBS absPath
            Plugin.afterZettelRead plugins mdFileTree
  log D $ "Building zettelkasten graph (" <> show (length zidRefs) <> " zettels) ..."
  pure $
    runWriter $ do
      filesWithContent <-
        flip Map.traverseMaybeWithKey zidRefs $ \zid -> \case
          R.ZIDRef_Ambiguous fps -> do
            tell $ one (zid, ZettelIssue_Error $ ZettelError_AmbiguousID fps)
            pure Nothing
          R.ZIDRef_Available fp s pluginData ->
            pure $ Just (fp, (s, pluginData))
      let zs = Plugin.afterZettelParse plugins (Map.toList filesWithContent)
      g <- G.buildZettelkasten zs
      pure (g, zs)
