{-# LANGUAGE BangPatterns #-}
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

module Neuron.Plugin.Plugins.DirTree (plugin, routePluginData, renderPanel) where

import qualified Data.Dependent.Map as DMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Some (Some (Some))
import Data.TagTree (Tag, TagNode (..), TagQuery (..), mkDefaultTagQuery)
import qualified Data.TagTree as Tag
import qualified Data.Text as T
import qualified Neuron.Frontend.Query.View as Q
import Neuron.Frontend.Route (NeuronWebT)
import Neuron.Frontend.Route.Data.Types
import Neuron.Frontend.Widget
  ( ListItem (ListItem_File, ListItem_Folder),
    listItem,
  )
import Neuron.Plugin.PluginData
  ( DirZettel (..),
    PluginZettelData (PluginZettelData_DirTree),
  )
import Neuron.Plugin.Type (Plugin (..))
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

-- Directory zettels using this plugin are associated with a `Tag` that
-- corresponds to the directory contents.
plugin :: Plugin DirZettelVal
plugin =
  def
    { _plugin_afterZettelRead = injectDirectoryZettels,
      _plugin_afterZettelParse = bimap addTagAndQuery addTagAndQuery,
      _plugin_renderPanel = renderPanel
    }

routePluginData :: ZettelGraph -> DirZettel -> DirZettelVal
routePluginData g DirZettel {..} =
  let childrenQuery = mkDefaultTagQuery $ one $ Tag.mkTagPatternFromTag _dirZettel_childrenTag
      children = Q.zettelsByTag (G.getZettels g) childrenQuery
      mparent = flip G.getZettel g =<< _dirZettel_dirParent
   in DirZettelVal children mparent

renderPanel :: (DomBuilder t m, PostBuild t m) => DirZettelVal -> NeuronWebT t m ()
renderPanel DirZettelVal {..} = do
  elClass "nav" "ui attached deemphasized segment dirfolge" $ do
    el "h3" $ text "Directory contents"
    divClass "ui list" $ do
      whenJust dirZettelValParent $ \parZ ->
        listItem ListItem_Folder $
          Q.renderZettelLink (Just $ text "..") Nothing Nothing parZ
      forM_ dirZettelValChildren $ \cz ->
        listItem (bool ListItem_File ListItem_Folder $ isDirectoryZettel cz) $
          Q.renderZettelLink Nothing (Just Folgezettel) Nothing cz

addTagAndQuery :: forall c. HasCallStack => ZettelT c -> ZettelT c
addTagAndQuery z =
  z
    { zettelTags =
        zettelTags z `Set.union` dirZettelTags,
      -- Add the tag query for building graph connections.
      zettelQueries =
        zettelQueries z <> fmap (,mempty) (maybeToList childrenTagQuery)
    }
  where
    dirZettelTags = fromMaybe Set.empty $ do
      case runIdentity <$> DMap.lookup PluginZettelData_DirTree (zettelPluginData z) of
        Just DirZettel {..} ->
          pure _dirZettel_tags
        Nothing ->
          -- Regular zettel
          pure $ maybe Set.empty Set.singleton $ parentDirTag $ zettelPath z
    childrenTagQuery = do
      DirZettel {..} <- runIdentity <$> DMap.lookup PluginZettelData_DirTree (zettelPluginData z)
      let q = TagQuery_Or $ one $ Tag.mkTagPatternFromTag _dirZettel_childrenTag
      pure $ Some $ ZettelQuery_ZettelsByTag q Folgezettel def

injectDirectoryZettels :: MonadState (Map ZettelID ZIDRef) m => DC.DirTree FilePath -> m ()
injectDirectoryZettels = \case
  DC.DirTree_Dir absPath contents -> do
    let dirName = takeFileName absPath
        dirZettelId = ZettelID $ if dirName == "." then indexZettelName else toText dirName
        parDirName = takeFileName (takeDirectory absPath)
        parDirZettelId = ZettelID $ if parDirName == "." then indexZettelName else toText parDirName
        mkPluginData tags = DMap.singleton PluginZettelData_DirTree $ Identity $ DirZettel tags (tagFromPath absPath) (Just parDirZettelId)
    -- Don't create folgezettel from index zettel. Why?
    -- - To avoid surprise when legacy notebooks with innuermous top level
    -- zettels use this feature.
    -- - Facilitate multiple clusters that don't "stick" because of index-folgezettel.
    -- If the user wants to make index branch to these top-level zettels,
    -- they can add `[[[z:zettels?tag=root]]]` to do that.
    unless (dirZettelId == ZettelID indexZettelName) $ do
      gets (Map.lookup dirZettelId) >>= \case
        Just (ZIDRef_Available p s pluginDataPrev) -> do
          -- A zettel with this directory name was already registered. Deal with it.
          case runIdentity <$> DMap.lookup PluginZettelData_DirTree pluginDataPrev of
            Just _ -> do
              -- A *directory* zettel of this name was already added.
              -- Ambiguous directories disallowed! For eg., you can't have
              -- Foo/Qux and Bar/Qux.
              R.markAmbiguous dirZettelId $ absPath :| [p]
            Nothing -> do
              -- A file zettel (DIRNAME.md) already exists on disk. Merge with it.
              let pluginData = mkPluginData $ Set.fromList $ catMaybes [parentDirTag absPath, parentDirTag p]
                  newRef = ZIDRef_Available p s (DMap.union pluginData pluginDataPrev)
              modify $ Map.update (const $ Just newRef) dirZettelId
        Just ZIDRef_Ambiguous {} ->
          -- TODO: What to do here?
          pure ()
        Nothing -> do
          -- Inject a new zettel corresponding to this directory, that is uniquely named.
          let pluginData = mkPluginData $ maybe Set.empty Set.singleton $ parentDirTag absPath
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
  "./" ->
    Nothing
  "./index.md" ->
    Nothing
  relPath ->
    Just $ tagFromPath $ takeDirectory relPath

tagFromPath :: HasCallStack => FilePath -> Tag
tagFromPath = \case
  "." ->
    -- Root file
    Tag.constructTag (rootTag :| [])
  ('.' : '/' : dirPath) ->
    let tagNodes = TagNode <$> T.splitOn "/" (T.replace " " "-" $ toText dirPath)
     in Tag.constructTag $ rootTag :| tagNodes
  relPath ->
    error $ "Invalid relPath passed to parseZettel: " <> toText relPath

-- TODO: Replace with indexZid from ID.hs
indexZettelName :: Text
indexZettelName = "index"

rootTag :: TagNode
rootTag = TagNode "root"

isDirectoryZettel :: ZettelT content -> Bool
isDirectoryZettel (zettelPluginData -> pluginData) =
  case DMap.lookup PluginZettelData_DirTree pluginData of
    Just _ -> True
    _ -> False
