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

import Colog (WithLog, log)
import Control.Monad.Fix (MonadFix)
import Control.Monad.Writer.Strict (runWriter, tell)
import Data.Dependent.Sum (DSum ((:=>)))
import Data.List (nubBy)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time (NominalDiffTime)
import Neuron.CLI.Logging
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
import qualified Neuron.Frontend.Manifest as Manifest
import Neuron.Frontend.Route (Route (Route_Impulse), routeHtmlPath)
import qualified Neuron.Frontend.Route as Z
import qualified Neuron.Frontend.Route.Data as RD
import qualified Neuron.Frontend.Static.HeadHtml as HeadHtml
import qualified Neuron.Frontend.Static.Html as Html
import qualified Neuron.Frontend.Widget as W
import Neuron.Plugin (PluginRegistry)
import qualified Neuron.Plugin as Plugin
import qualified Neuron.Plugin.Plugins.NeuronIgnore as NeuronIgnore
import Neuron.Version (neuronVersion)
import qualified Neuron.Zettelkasten.Graph as G
import qualified Neuron.Zettelkasten.Graph.Build as G
import Neuron.Zettelkasten.Graph.Type (ZettelGraph, stripSurroundingContext)
import Neuron.Zettelkasten.ID (ZettelID (..))
import qualified Neuron.Zettelkasten.Resolver as R
import Neuron.Zettelkasten.Zettel (ZettelC)
import qualified Neuron.Zettelkasten.Zettel as Z
import Neuron.Zettelkasten.Zettel.Error
  ( ZettelError (..),
    ZettelIssue (..),
    splitZettelIssues,
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
import System.Directory (doesFileExist, makeAbsolute, withCurrentDirectory)
import qualified System.Directory.Contents as DC
import qualified System.Directory.Contents.Extra as DC
import qualified System.FSNotify as FSN
import System.FilePath (isRelative, makeRelative, takeExtension, (</>))

generateSite :: Bool -> App ()
generateSite continueMonitoring = do
  -- Initial gen
  doGen
  -- Continue morning
  let awaitLog = log (D' Wait) "Awaiting file changes"
  when continueMonitoring $ do
    awaitLog
    appEnv <- getAppEnv
    notesDir <- getNotesDir
    liftIO $
      runHeadlessApp $ do
        fsChanged <- watchDirWithDebounce 0.1 notesDir
        performEvent_ $
          ffor fsChanged $ \(fmap FSN.eventPath -> paths) -> do
            liftIO $
              runApp appEnv $ do
                forM_ paths $ \path ->
                  log (I' Received) $ toText path
                doGen
                awaitLog
        pure never
  where
    -- Do a one-off generation from top to bottom.
    -- No incrementall stuff (yet)
    doGen :: App ()
    doGen = do
      (cache, zs, fileTree) <- loadZettelkasten =<< getConfig
      -- Static files
      case DC.walkContents "static" fileTree of
        Just staticTree@(DC.DirTree_Dir _ _) -> do
          notesDir <- getNotesDir
          outputDir <- getOutputDir
          DC.rsyncDir notesDir outputDir staticTree
        _ ->
          pure ()
      -- Build all routes, and their data
      headHtml <- HeadHtml.getHeadHtmlFromTree fileTree
      let manifest = Manifest.mkManifestFromTree fileTree
          siteData = RD.mkSiteData cache headHtml manifest
          impulseData = RD.mkImpulseData cache
          impulseRouteData = (siteData, impulseData)
          mkZettelRoute zC =
            let z = Z.sansContent zC
                zettelData = RD.mkZettelData cache zC
             in Z.Route_Zettel (Z.zettelSlug z) :=> Identity (siteData, zettelData)
          routes :: [DSum Route Identity] =
            (Z.Route_Impulse Nothing :=> Identity impulseRouteData) :
            (Z.Route_ImpulseStatic :=> Identity impulseRouteData) :
            fmap mkZettelRoute zs
      -- Render and write the routes
      log D $ "Rendering routes (" <> show (length routes) <> " slugs) ..."
      _ :: [()] <- forM routes $ \case
        r@(Z.Route_Zettel _) :=> Identity val ->
          writeRouteHtml r =<< genRouteHtml r val
        r@(Z.Route_Impulse _) :=> Identity val ->
          writeRouteHtml r =<< genRouteHtml r val
        r@Z.Route_ImpulseStatic :=> Identity val ->
          writeRouteHtml r =<< genRouteHtml r val
      -- Report any errors and finish.
      reportAllErrors (Cache._neuronCache_graph cache) (Cache._neuronCache_errors cache)
      getOutputDir >>= \(toText -> outputDir) ->
        log (I' Done) $ "Finished generating at " <> outputDir

-- Report all errors
-- TODO: Report only new errors in this run, to avoid spamming the terminal.
reportAllErrors ::
  forall m env.
  (MonadIO m, WithLog env Message m) =>
  ZettelGraph ->
  Map ZettelID ZettelIssue ->
  m ()
reportAllErrors g issues = do
  let (errors, badLinks) = splitZettelIssues issues
  whenNotNull badLinks $ \_ -> do
    let zn = length badLinks
        ln = length $ concat $ toList . snd . snd <$> badLinks
    if zn < 3
      then forM_ badLinks $ \(zid, _qErrs) -> do
        let path = maybe "??" Z.zettelPath $ G.getZettel zid g
        log W $ "Missing link in " <> toText path
      else log W $ show ln <> " missing links found across " <> show zn <> " notes (see Impulse)"
  uncurry reportError `mapM_` errors
  where
    -- Report an error in the terminal
    reportError :: ZettelID -> ZettelError -> m ()
    reportError zid (zettelErrorText -> err) = do
      -- We don't know the full path to this zettel; so just print the ID.
      log EE $ "Cannot accept Zettel ID: " <> unZettelID zid
      log E $ indentAllButFirstLine 4 err
      where
        indentAllButFirstLine :: Int -> Text -> Text
        indentAllButFirstLine n = T.strip . unlines . go . lines
          where
            go [] = []
            go [x] = [x]
            go (x : xs) =
              x : fmap (toText . (replicate n ' ' <>) . toString) xs

genRouteHtml ::
  forall m a.
  MonadIO m =>
  Route a ->
  a ->
  m ByteString
genRouteHtml r val = do
  -- We do this verbose dance to make sure hydration happens only on Impulse route.
  -- Ideally, this should be abstracted out, but polymorphic types are a bitch.
  liftIO $ case r of
    Route_Impulse {} ->
      fmap snd . renderStatic . runHydratableT $ do
        -- FIXME: Injecting initial value here will break hydration on Impulse.
        let valDyn = constDyn $ W.unavailableData @a
        Html.renderRoutePage r valDyn
    _ ->
      fmap snd . renderStatic $ do
        let valDyn = constDyn $ W.availableData val
        Html.renderRoutePage r valDyn

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
    log (I' Sent) $ toText $ DC.mkRelative outputDir htmlFile
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
    MonadIO m,
    MonadFix m
  ) =>
  NominalDiffTime ->
  FilePath ->
  m (Event t [FSEvent])
watchDirWithDebounce ms dirPath' = do
  -- Make absolute herte, so the relative function below works.
  dirPath <- liftIO $ makeAbsolute dirPath'
  let cfg = FSN.defaultConfig {FSN.confDebounce = FSN.Debounce ms}
  pb <- getPostBuild
  fsEvt <- watchTree cfg (dirPath <$ pb) (const True)
  let evt2 = fforMaybe fsEvt $ \fse' -> do
        fse <- mkEventPathRelative dirPath fse'
        let path = FSN.eventPath fse
            ign = NeuronIgnore.shouldIgnore NeuronIgnore.mandatoryIgnorePats
        guard $ not $ ign path
        pure fse
  evtGrouped <- fmap toList <$> batchOccurrences ms evt2
  -- Discard all but the last event for each path.
  pure $ nubByKeepLast ((==) `on` FSN.eventPath) <$> evtGrouped
  where
    -- Like @Data.List.nubBy@ but keeps the last occurence
    nubByKeepLast :: (a -> a -> Bool) -> [a] -> [a]
    nubByKeepLast f =
      reverse . nubBy f . reverse
    mkEventPathRelative :: FilePath -> FSN.Event -> Maybe FSN.Event
    mkEventPathRelative baseDir = \case
      FSN.Added fp t d ->
        mkR fp <&> \p -> FSN.Added p t d
      FSN.Modified fp t d ->
        mkR fp <&> \p -> FSN.Modified p t d
      FSN.Removed fp t d ->
        mkR fp <&> \p -> FSN.Removed p t d
      FSN.Unknown fp t d ->
        mkR fp <&> \p -> FSN.Unknown p t d
      where
        mkR fp = do
          let rel = makeRelative baseDir fp
          guard $ isRelative rel
          -- Tack in a "./" to be directory-contents friendly
          pure $ "./" <> rel

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
  log D $ "Building graph (" <> show (length zidRefs) <> " notes) ..."
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
