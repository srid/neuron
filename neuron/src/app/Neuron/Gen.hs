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

-- TODO: Rename to Neuron.Reactor?
-- ie. Name an internal component for mnemonic benefits.
module Neuron.Gen where

import Control.Monad.Fix (MonadFix)
import Control.Monad.Writer.Strict (runWriter, tell)
import Data.List (nubBy)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Text.IO (hPutStr, hPutStrLn)
import Data.Time (NominalDiffTime)
import Neuron.CLI.Types
  ( App (..),
    AppT,
    MonadApp (..),
    getApp,
    runAppT,
  )
import Neuron.Config (getConfig)
import Neuron.Config.Type (Config)
import qualified Neuron.Config.Type as Config
import Neuron.Plugin (PluginRegistry)
import qualified Neuron.Plugin as Plugin
import Neuron.Version (neuronVersion)
import qualified Neuron.Web.Cache as Cache
import Neuron.Web.Cache.Type (NeuronCache)
import qualified Neuron.Web.Cache.Type as Cache
import qualified Neuron.Web.HeadHtml as HeadHtml
import qualified Neuron.Web.Html as Html
import Neuron.Web.Manifest (Manifest)
import qualified Neuron.Web.Manifest as Manifest
import Neuron.Web.Route (Route (Route_Impulse), routeHtmlPath)
import qualified Neuron.Web.Route as Z
import qualified Neuron.Web.Route.Data as RD
import qualified Neuron.Web.Widget as W
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
import System.FilePath ((</>))

generateSite :: AppT IO ()
generateSite = do
  app <- getApp
  --initial gen
  doGen
  liftIO $ putStrLn "Monitoring for changes ..."
  lift $
    runHeadlessApp $ do
      fsChanged <- genApp app
      performEvent_ $
        ffor fsChanged $ \(fmap FSN.eventPath -> paths) -> do
          forM_ paths $ \path ->
            liftIO $ putStrLn $ "M " <> DC.mkRelative (notesDir app) path
          liftIO $ runAppT app doGen
          liftIO $ putStrLn "Monitoring for changes ..."
      pure never
  where
    -- Do a one-off generation from top to bottom.
    -- No incrementall stuff (yet)
    doGen :: AppT IO ()
    doGen = do
      (cache, RD.mkRouteDataCache -> rdCache, fileTree) <- loadZettelkasten =<< getConfig
      -- Static files
      case DC.walkContents "static" fileTree of
        Just staticTree@(DC.DirTree_Dir _ _) -> do
          notesDir <- getNotesDir
          outputDir <- getOutputDir
          liftIO $ DC.dirTreeCopy notesDir outputDir staticTree
        _ ->
          pure ()
      headHtml <- HeadHtml.getHeadHtmlFromTree fileTree
      let manifest = Manifest.mkManifestFromTree fileTree
      -- Do it
      let wH :: Route a -> AppT IO ()
          wH r = writeRouteHtml r =<< genRouteHtml headHtml manifest cache rdCache r
      (wH . Z.Route_Zettel) `mapM_` RD.allSlugs rdCache
      wH $ Z.Route_Impulse Nothing
      wH Z.Route_ImpulseStatic
      reportAllErrors cache
      pure ()

    genApp ::
      forall t m.
      ( Reflex t,
        MonadIO m,
        PerformEvent t m,
        PostBuild t m,
        TriggerEvent t m,
        MonadHold t m,
        MonadFix m,
        MonadIO (Performable m)
      ) =>
      App ->
      m (Event t [FSEvent])
    genApp App {..} = do
      -- TODO: Not doing any change monitoring for now; while we migrate away from rib.
      watchDirWithDebounce 0.1 notesDir

-- Report all errors
-- TODO: Report only new errors in this run, to avoid spamming the terminal.
reportAllErrors :: MonadIO m => NeuronCache -> m ()
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
    liftIO $ hPutStrLn stderr $ "E " <> show missingLinks <> " missing links found across zettels (see Impulse)"
  where
    -- Report an error in the terminal
    reportError :: MonadIO m => ZettelID -> ZettelError -> m ()
    reportError zid (zettelErrorText -> err) = do
      liftIO $ hPutStrLn stderr $ toText $ "E " <> zettelIDSourceFileName zid
      liftIO $ hPutStr stderr $ "  - " <> indentAllButFirstLine 4 err
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

writeRouteHtml :: (MonadApp m, MonadIO m) => Route a -> ByteString -> m ()
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
    liftIO $ putStrLn $ "+ " <> DC.mkRelative outputDir htmlFile
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
        not (".neuron" `T.isInfixOf` path || ".git" `T.isInfixOf` path)
  evtGrouped <- fmap toList <$> batchOccurrences ms evt2
  -- Discard all but the last event for each path.
  pure $ nubByKeepLast ((==) `on` FSN.eventPath) <$> evtGrouped
  where
    -- Like @Data.List.nubBy@ but keeps the last occurence
    nubByKeepLast :: (a -> a -> Bool) -> [a] -> [a]
    nubByKeepLast f =
      reverse . nubBy f . reverse

-- Functions from old Generate.hs

loadZettelkasten :: (MonadIO m, MonadApp m) => Config -> m (NeuronCache, [ZettelC], DC.DirTree FilePath)
loadZettelkasten config = do
  let plugins = Config.getPlugins config
  liftIO $ hPutStrLn stderr $ "Plugins enabled: " <> show (Map.keys plugins)
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
  (MonadIO m, MonadApp m) =>
  PluginRegistry ->
  DC.DirTree FilePath ->
  m
    ( ( ZettelGraph,
        [ZettelC]
      ),
      Map ZettelID ZettelIssue
    )
loadZettelkastenFromFiles plugins fileTree = do
  let total = getSum @Int $ foldMap (const $ Sum 1) fileTree
  -- TODO: Would be nice to show a progressbar here
  -- liftIO $ DC.printDirTree fileTree
  liftIO $ hPutStrLn stderr $ "Loading directory tree (" <> show total <> " files) ..."
  zidRefs <-
    fmap snd $
      flip runStateT Map.empty $ do
        flip R.resolveZidRefsFromDirTree fileTree $ \relPath -> do
          -- NOTE: This is the only place where Shake is being used (for
          -- posterity)
          absPath <- fmap (</> relPath) getNotesDir
          needFile absPath
          decodeUtf8With lenientDecode <$> readFileBS absPath
        Plugin.afterZettelRead plugins fileTree
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
