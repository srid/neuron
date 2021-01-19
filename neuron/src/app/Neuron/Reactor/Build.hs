{-# LANGUAGE BangPatterns #-}
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
module Neuron.Reactor.Build where

import Colog (WithLog, log)
import Control.Monad.Writer.Strict (runWriter, tell)
import Data.Dependent.Sum (DSum ((:=>)))
import qualified Data.Map.Strict as Map
import Data.Some (Some (Some), foldSome)
import Neuron.CLI.Logging
import Neuron.CLI.Types
import qualified Neuron.Cache as Cache
import Neuron.Cache.Type (NeuronCache)
import qualified Neuron.Cache.Type as Cache
import qualified Neuron.Config as Config
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
import Neuron.Version (neuronVersion)
import qualified Neuron.Zettelkasten.Graph.Build as G
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
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
import Reflex.Dom.Core
import Relude
import System.Directory (doesFileExist, withCurrentDirectory)
import qualified System.Directory.Contents as DC
import qualified System.Directory.Contents.Extra as DC
import System.FilePath (takeExtension, (</>))

writeRoutes :: NonEmpty (DSum Route Identity) -> App Int
writeRoutes new = do
  log D $ "Rendering routes (" <> show (length new) <> " slugs) ..."
  -- TODO: Reflex static renderer is our main bottleneck. Memoize it using route data.
  -- Second bottleneck is graph building.
  !routeHtml <- forM new $ \case
    r@(Z.Route_Zettel _) :=> Identity val -> do
      (Some r,) <$> genRouteHtml r val
    r@(Z.Route_Impulse _) :=> Identity val ->
      (Some r,) <$> genRouteHtml r val
    r@Z.Route_ImpulseStatic :=> Identity val ->
      (Some r,) <$> genRouteHtml r val
  -- log D "Writing to disk ..."
  fmap sum $
    forM routeHtml $ \(someR, html) -> do
      fmap (bool 0 1) $ flip writeRouteHtml html `foldSome` someR

copyStaticFiles :: (MonadApp m, MonadIO m, WithLog env Message m) => DC.DirTree FilePath -> m Int
copyStaticFiles fileTree = do
  case DC.walkContents "static" fileTree of
    Just staticTree@(DC.DirTree_Dir _ _) -> do
      notesDir <- getNotesDir
      outputDir <- getOutputDir
      DC.rsyncDir notesDir outputDir staticTree
    _ ->
      pure 0

buildRouteData :: (MonadIO m, MonadApp m, WithLog env Message m) => DC.DirTree FilePath -> [ZettelC] -> NeuronCache -> m [DSum Route Identity]
buildRouteData fileTree zs cache = do
  log D "Building route data ..."
  headHtml <- HeadHtml.getHeadHtmlFromTree fileTree
  let manifest = Manifest.mkManifestFromTree fileTree
      siteData = RD.mkSiteData cache headHtml manifest
      impulseData = RD.mkImpulseData cache
      impulseRouteData = (siteData, impulseData)
      mkZettelRoute zC =
        let z = Z.sansContent zC
            zettelData = RD.mkZettelData cache zC
         in Z.Route_Zettel (Z.zettelSlug z) :=> Identity (siteData, zettelData)
      !routes =
        (Z.Route_Impulse Nothing :=> Identity impulseRouteData) :
        (Z.Route_ImpulseStatic :=> Identity impulseRouteData) :
        fmap mkZettelRoute zs
  pure routes

-- Report all errors
-- TODO: Report only new errors in this run, to avoid spamming the terminal.
reportAllErrors ::
  forall m env.
  (MonadIO m, WithLog env Message m) =>
  Map ZettelID ZettelIssue ->
  m ()
reportAllErrors issues = do
  let (errors, badLinks) = splitZettelIssues issues
  whenNotNull badLinks $ \_ -> do
    let zn = length badLinks
        ln = length $ concat $ toList . snd . snd <$> badLinks
    log W $ show ln <> " missing links found across " <> show zn <> " notes (see Impulse)"
  uncurry reportError `mapM_` errors
  where
    -- Report an error in the terminal
    reportError :: ZettelID -> ZettelError -> m ()
    reportError zid (zettelErrorText -> err) = do
      -- We don't know the full path to this zettel; so just print the ID.
      log EE $ "Cannot accept Zettel ID: " <> unZettelID zid
      log E $ indentAllButFirstLine 4 err

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

writeRouteHtml :: (MonadApp m, MonadIO m, WithLog env Message m) => Route a -> ByteString -> m Bool
writeRouteHtml r content = do
  outputDir <- getOutputDir
  let htmlFile = outputDir </> routeHtmlPath r
  -- DOCTYPE declaration is helpful for code that might appear in the user's `head.html` file (e.g. KaTeX).
  let s = decodeUtf8 @Text $ "<!DOCTYPE html>" <> content
  s0 <- liftIO $ do
    doesFileExist htmlFile >>= \case
      True -> Just <$> readFileText htmlFile
      False -> pure Nothing
  if Just s /= s0
    then do
      log (I' Sent) $ toText $ DC.mkRelative outputDir htmlFile
      writeFileText htmlFile s
      pure True
    else pure False

-- Functions from old Generate.hs

loadZettelkasten ::
  (MonadIO m, MonadApp m, WithLog env Message m) =>
  m (Either Text (NeuronCache, [ZettelC], DC.DirTree FilePath))
loadZettelkasten = do
  locateZettelFiles >>= \case
    Left e -> pure $ Left e
    Right (config, fileTree) ->
      Right <$> loadZettelkastenFromFiles config fileTree

loadZettelkastenFromFiles :: (WithLog env Message m, MonadApp m, MonadIO m) => Config -> DC.DirTree FilePath -> m (NeuronCache, [ZettelC], DC.DirTree FilePath)
loadZettelkastenFromFiles config fileTree = do
  let plugins = Config.getPlugins config
  log D $ "Plugins enabled: " <> Plugin.pluginRegistryShow plugins
  ((g, zs), errs) <- loadZettelkastenFromFilesWithPlugins plugins fileTree
  let cache = Cache.NeuronCache g errs config neuronVersion
  Cache.updateCache $ Cache.stripCache cache
  pure (cache, zs, fileTree)

locateZettelFiles :: (MonadIO m, MonadApp m) => m (Either Text (Config, DC.DirTree FilePath))
locateZettelFiles = do
  -- Run with notes dir as PWD, so that DirTree uses relative paths throughout.
  d <- getNotesDir
  liftIO $
    withCurrentDirectory d $
      -- Building with "." as root directory ensures that the returned tree
      -- structure consistently uses the "./" prefix for all paths. This
      -- assumption then holds elsewhere in neuron.
      DC.buildDirTree "." >>= \case
        Just t -> do
          -- Look for neuron.dhall
          case DC.walkContents "neuron.dhall" t of
            Just (DC.DirTree_File _ dhallFp) -> do
              Config.getConfigFromFile dhallFp >>= \case
                Left e ->
                  pure $ Left e
                Right config ->
                  Plugin.filterSources (Config.getPlugins config) t >>= \case
                    Nothing ->
                      pure $ Left "No source files to process"
                    Just tF ->
                      pure $ Right (config, tF)
            _ ->
              pure $ Left $ Config.missingConfigError d
        Nothing ->
          pure $ Left "Empty directory"

-- | Load the Zettelkasten from disk, using the given list of zettel files
loadZettelkastenFromFilesWithPlugins ::
  (MonadIO m, MonadApp m, WithLog env Message m) =>
  PluginRegistry ->
  DC.DirTree FilePath ->
  m
    ( ( ZettelGraph,
        [ZettelC]
      ),
      Map ZettelID ZettelIssue
    )
loadZettelkastenFromFilesWithPlugins plugins fileTree = do
  !zidRefs <-
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
      let !zs = Plugin.afterZettelParse plugins (Map.toList filesWithContent)
      !g <- G.buildZettelkasten plugins zs
      pure (g, zs)
