{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Plugin.Plugins.DirTree (plugin, renderPanel) where

import Control.Monad.Writer (runWriter)
import Control.Monad.Writer.Strict (MonadWriter (tell))
import qualified Data.Dependent.Map as DMap
import qualified Data.Map.Strict as Map
import Data.Some (Some (Some))
import Data.TagTree (Tag (Tag), mkTagPatternFromTag)
import qualified Data.Text as T
import Neuron.Plugin.PluginData (PluginData (PluginData_DirTree))
import Neuron.Plugin.Type (Plugin (..))
import qualified Neuron.Web.Query.View as Q
import Neuron.Web.Route (NeuronWebT)
import Neuron.Zettelkasten.Connection (Connection (Folgezettel))
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.ID (ZettelID (ZettelID))
import qualified Neuron.Zettelkasten.Query as Q
import Neuron.Zettelkasten.Resolver (ZIDRef (..))
import qualified Neuron.Zettelkasten.Resolver as R
import Neuron.Zettelkasten.Zettel (ZettelQuery (..), ZettelT (..))
import Reflex.Dom.Core
import Relude hiding (trace, traceShow, traceShowId)
import qualified System.Directory.Contents as DC
import System.FilePath (takeDirectory, takeFileName)
import System.FilePath.Posix (takeExtension)
import System.FilePattern (FilePattern, (?==))

-- Directory zettels using this plugin are associated with a `Tag` that
-- corresponds to the directory contents.
plugin :: Plugin (Tag, Maybe ZettelID)
plugin =
  Plugin
    { _plugin_filterSources = applyNeuronIgnore,
      _plugin_afterZettelRead = injectDirectoryZettels,
      _plugin_afterZettelParse = bimap addTagAndQuery addTagAndQuery,
      _plugin_renderPanel = renderPanel
    }

-- | Ignore files based on the top-level .neuronignore file. If the file does
-- not exist, apply the default patterns.
applyNeuronIgnore :: DC.DirTree FilePath -> IO (Maybe (DC.DirTree FilePath))
applyNeuronIgnore t = do
  -- Note that filterDirTree invokes the function only files, not directory paths
  -- FIXME: `filterADirTree` unfortunately won't filter directories; so
  -- even if a top-level directory is configured to be ignored, this
  -- filter will traverse that entire directory tree to apply the glob
  -- pattern filter.
  -- TODO: neuron should detect changes to this file, and reload.
  ignorePats :: [FilePattern] <- case DC.walkDirTree "./.neuronignore" t of
    Just (DC.DirTree_File _ fp) -> do
      ls <- T.lines <$> readFileText fp
      pure $
        fforMaybe ls $ \(T.strip -> s) -> do
          guard $ not $ T.null s
          -- Ignore line comments
          guard $ not $ "#" `T.isPrefixOf` s
          pure $ toString s
    _ ->
      pure defaultIgnorePats
  putTextLn $ "Ignore patterns: " <> show ignorePats
  let (mTreeFiltered, _nExcluded) = runWriter @[FilePath] $ DC.filterADirTree (includeDirEntry ignorePats) t
  -- Not printing, because this includesd all non-Markdown files, including static files. Hence, not really accurate.
  -- liftIO $ putStrLn $ "Excluded " <> show nExcluded <> " filepaths"
  pure $ DC.pruneDirTree =<< mTreeFiltered
  where
    defaultIgnorePats =
      [ -- Ignore top-level dotfiles and dotdirs
        "./.*/**"
        -- Ignore everything under sub directories
        -- "./*/*/**"
      ]
    includeDirEntry pats name = do
      let included =
            Just True
              == ( do
                     guard $ not $ any (?== name) pats
                     guard $ takeExtension name == ".md"
                     pure True
                 )
      unless included $ tell [name]
      pure included

renderPanel :: DomBuilder t m => ZettelGraph -> (Tag, Maybe ZettelID) -> NeuronWebT t m ()
renderPanel graph (t, mpar) = do
  elClass "nav" "ui attached deemphasized segment dirfolge" $ do
    el "h3" $ text "Directory contents:"
    -- XXX: Display ".." link even on ordinary zettels? Probably not; that's
    -- what uplink tree is for.
    divClass "ui list" $ do
      let children = Q.zettelsByTag (G.getZettels graph) [mkTagPatternFromTag t]
      whenJust (mpar >>= flip G.getZettel graph) $ \parZ ->
        divClass "item" $ do
          elClass "i" "folder icon" blank
          divClass "content" $
            divClass "description" $
              Q.renderZettelLink (Just $ text "..") Nothing Nothing parZ
      forM_ children $ \cz ->
        divClass "item" $ do
          let ico = bool "file outline icon" "folder icon" $ isDirectoryZettel cz
          elClass "i" ico blank
          divClass "content" $
            divClass "description" $
              Q.renderZettelLink Nothing (Just Folgezettel) Nothing cz

addTagAndQuery :: forall c. HasCallStack => ZettelT c -> ZettelT c
addTagAndQuery z =
  z
    { -- TODO: There should be multiple tags, for other parents. cf. my
      -- "iDrinkCoffee" folder placed elsewhere.
      zettelTags =
        zettelTags z <> maybeToList (parentDirTag $ zettelPath z),
      -- Add the tag query for building graph connections.
      zettelQueries =
        zettelQueries z <> fmap (,mempty) (maybeToList tagQuery)
    }
  where
    tagQuery :: Maybe (Some ZettelQuery)
    tagQuery = do
      (t, _mpar) <- runIdentity <$> DMap.lookup PluginData_DirTree (zettelPluginData z)
      pure $ Some $ ZettelQuery_ZettelsByTag [mkTagPatternFromTag t] Folgezettel def

injectDirectoryZettels :: MonadState (Map ZettelID ZIDRef) m => DC.DirTree FilePath -> m ()
injectDirectoryZettels = \case
  DC.DirTree_Dir absPath contents -> do
    let dirName = takeFileName absPath
        dirZettelId = ZettelID $ toText $ if dirName == "." then "index" else dirName
        parDirName = takeFileName (takeDirectory absPath)
        parDirZettelId = ZettelID $ toText $ if parDirName == "." then "index" else parDirName
    -- Don't create folgezettel from index zettel. Why?
    -- - To avoid surprise when legacy notebooks with innuermous top level
    -- zettels use this feature.
    -- - Facilitate multiple clusters that don't "stick" because of index-folgezettel.
    -- If the user wants to make index branch to these top-level zettels,
    -- they can add `[[[z:zettels?tag=index]]]` to do that.
    unless (dirZettelId == ZettelID "index") $ do
      let dirTag = tagFromPath absPath
          pluginData = DMap.singleton PluginData_DirTree (Identity (dirTag, Just parDirZettelId))
      gets (Map.lookup dirZettelId) >>= \case
        Just ref -> do
          case ref of
            ZIDRef_Available p s pluginData' -> do
              -- If a zettel with the same name as the directory already exists,
              -- treat that zettel as the directory zettel, by adding this
              -- plugin's data to it.
              let newRef = ZIDRef_Available p s (DMap.union pluginData' pluginData)
              modify $ Map.update (const $ Just newRef) dirZettelId
            ZIDRef_Ambiguous {} ->
              -- TODO: What to do here?
              pure ()
        Nothing -> do
          -- Inject a new zettel corresponding to this directory.
          R.addZettel absPath dirZettelId pluginData $ do
            -- Set an appropriate title (same as directory name)
            let heading = toText (takeFileName absPath) <> "/"
            pure $ "# " <> heading
    forM_ (Map.toList contents) $ \(_, ct) ->
      injectDirectoryZettels ct
  _ ->
    pure ()

parentDirTag :: HasCallStack => FilePath -> Maybe Tag
parentDirTag = \case
  "./index.md" ->
    Nothing
  relPath ->
    Just $ tagFromPath $ takeDirectory relPath

tagFromPath :: HasCallStack => FilePath -> Tag
tagFromPath = \case
  "." ->
    -- Root file
    Tag indexZettelName
  ('.' : '/' : dirPath) ->
    Tag $ indexZettelName <> "/" <> T.replace " " "-" (toText dirPath)
  relPath ->
    error $ "Invalid relPath passed to parseZettel: " <> toText relPath
  where
    indexZettelName = "index"

isDirectoryZettel :: ZettelT content -> Bool
isDirectoryZettel (zettelPluginData -> pluginData) =
  case DMap.lookup PluginData_DirTree pluginData of
    Just _ -> True
    _ -> False
