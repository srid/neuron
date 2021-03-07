{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
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
module Neuron.Reactor
  ( generateSite,
    loadZettelkasten,
  )
where

import Colog (WithLog, log)
import Control.Monad.Fix (MonadFix)
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum ((:=>)))
import Data.List (nubBy)
import Data.Time (NominalDiffTime)
import Neuron.CLI.Logging
import Neuron.CLI.Types
import Neuron.Cache.Type (NeuronCache)
import qualified Neuron.Cache.Type as Cache
import Neuron.Config.Type (Config)
import Neuron.Frontend.Route
  ( Route (..),
  )
import qualified Neuron.Plugin.Plugins.NeuronIgnore as NeuronIgnore
import qualified Neuron.Reactor.Build as RB
import Neuron.Zettelkasten.Zettel (ZettelC)
import Reflex.Dom.Core
import Reflex.FSNotify (FSEvent, watchTree)
import Reflex.Host.Headless (runHeadlessApp)
import Relude
import System.Directory (makeAbsolute)
import qualified System.Directory.Contents as DC
import qualified System.FSNotify as FSN
import System.FilePath (isRelative, makeRelative)

generateSite :: GenCommand -> App ()
generateSite genCmd = do
  appEnv <- getAppEnv
  liftIO $
    runHeadlessApp $ do
      generated <- reflexApp appEnv genCmd
      pure $ bool generated never (watch genCmd)

reflexApp ::
  forall m t.
  (MonadIO m, PerformEvent t m, MonadFix m, MonadHold t m, MonadIO (Performable m), TriggerEvent t m, PostBuild t m, Adjustable t m, NotReady t m) =>
  Env App ->
  GenCommand ->
  m (Event t ())
reflexApp appEnv genCmd = do
  let run :: forall m1 a. MonadIO m1 => App a -> m1 a
      run a = liftIO $ runApp appEnv a
  -- Build a dynamic of directory tree
  treeOrErrorDyn <- eitherDyn =<< buildDirTreeDyn appEnv
  switchHold never <=< dyn $
    ffor treeOrErrorDyn $ \case
      Left errDyn -> do
        -- Display neuron.dhall errors
        dyn $
          ffor errDyn $ \err ->
            run $ do
              log EE "Config file error"
              log E $ indentAllButFirstLine 4 err
      Right treeDyn -> do
        -- Build route data
        routeDataWithErrorsE <- dyn $
          ffor treeDyn $ \(cfg, tree') -> do
            (cache, zs, fileTree) <- run $ RB.loadZettelkastenFromFiles cfg tree'
            routeData <- run $ RB.buildRouteData (RB.neuronRouteConfig genCmd) fileTree zs cache
            pure (routeData, Cache.neuroncacheErrors cache)
        -- Remember last route data, so we know what to render to save time.
        routeDataWithErrorsE' <- rememberLastEvent (mempty, mempty) routeDataWithErrorsE
        -- Do everything now.
        done <- performEvent $
          ffor (attach (current $ snd <$> treeDyn) routeDataWithErrorsE') $ \(filesTree, ((oldRoutes, _oldErrs), (newRoutes, newErrors))) -> do
            -- Copy static files
            nStatic <- run $ RB.copyStaticFiles filesTree
            let routeChanges = whatChanged oldRoutes newRoutes
            -- Deleted removed routes if any
            unless (null oldRoutes) $ do
              case nonEmpty (deletions routeChanges) of
                Just rs -> run $ RB.deleteRoutes rs
                Nothing -> pure ()
            -- Write modified routes
            nRoutes <- case nonEmpty (modifications routeChanges) of
              Just rs -> run $ RB.writeRoutes genCmd (DMap.fromList newRoutes) rs
              Nothing -> pure 0
            -- Report errors
            run $ RB.reportAllErrors newErrors
            pure $ nRoutes + nStatic
        -- Report finish status
        widgetHold_ blank $
          ffor done $ \n' ->
            run $ do
              outputDir <- getOutputDir
              case n' of
                0 -> log (I' Done) "Nothing updated in output directory"
                n -> log (I' Done) $ show n <> " files updated in " <> toText outputDir
        pure $ () <$ done
  where
    rememberLastEvent :: a -> Event t a -> m (Event t (a, a))
    rememberLastEvent x evt = do
      xDyn <- holdDyn x evt
      pure $ attach (current xDyn) evt

data Change a
  = Modified a
  | Deleted a
  | NoChange
  deriving (Eq, Show, Functor)

modifications :: [Change a] -> [a]
modifications = fmapMaybe $ \case
  Modified x -> Just x
  _ -> Nothing

deletions :: [Change a] -> [a]
deletions = fmapMaybe $ \case
  Deleted x -> Just x
  _ -> Nothing

whatChanged :: [DSum Route Identity] -> [DSum Route Identity] -> [Change (DSum Route Identity)]
whatChanged old@(DMap.fromList -> oldMap) new@(DMap.fromList -> newMap) =
  let needsRebuild :: forall a. Eq a => DMap Route Identity -> Route a -> Identity a -> Bool
      needsRebuild otherMap r val =
        case DMap.lookup r otherMap of
          Just otherVal
            | otherVal == val ->
              False
          _ -> True
      ms = fforMaybe new $ \case
        rd@(r@(Route_Zettel _) :=> newVal) -> do
          pure $ bool NoChange (Modified rd) $ needsRebuild oldMap r newVal
        rd@(r@Route_Impulse :=> newVal) -> do
          pure $ bool NoChange (Modified rd) $ needsRebuild oldMap r newVal
      ds = fforMaybe old $ \case
        rd@(r@(Route_Zettel _) :=> _) -> do
          pure $ bool NoChange (Deleted rd) $ isNothing $ DMap.lookup r newMap
        _ -> Nothing
   in ms <> ds

buildDirTreeDyn :: (MonadIO (Performable m), MonadIO m, PerformEvent t m, TriggerEvent t m, PostBuild t m, MonadHold t m, MonadFix m) => Env App -> m (Dynamic t (Either Text (Config, DC.DirTree FilePath)))
buildDirTreeDyn appEnv = do
  let run :: forall m1 a. MonadIO m1 => App a -> m1 a
      run a = liftIO $ runApp appEnv a
  (notesDir, tree0) <-
    run $ (,) <$> getNotesDir <*> RB.locateZettelFiles
  fsEventsE <- watchDirWithDebounce 0.1 notesDir
  treeE <- performEvent $
    ffor fsEventsE $ \fsEvents ->
      run $ do
        forM_ fsEvents $ \event ->
          case event of
            FSN.Removed {} ->
              log (I' Trashed) $ toText (FSN.eventPath event)
            _ ->
              log (I' Received) $ toText (FSN.eventPath event)
        -- TODO(perf): Instead of rebuilding the tree, patch the existing tree
        -- with changed paths.
        RB.locateZettelFiles
  holdDyn tree0 treeE

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

loadZettelkasten ::
  (MonadIO m, MonadApp m, WithLog env Message m) =>
  m (Either Text (NeuronCache, [ZettelC], DC.DirTree FilePath))
loadZettelkasten = RB.loadZettelkasten
