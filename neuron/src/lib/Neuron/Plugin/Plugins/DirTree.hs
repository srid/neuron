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
import Data.TagTree (Tag, TagNode (..), mkDefaultTagQuery)
import qualified Data.TagTree as Tag
import qualified Data.Text as T
import qualified Neuron.Frontend.Query.View as Q
import Neuron.Frontend.Route (NeuronWebT)
import Neuron.Frontend.Route.Data.Types (DirZettelVal (..))
import Neuron.Frontend.Widget
  ( ListItem (ListItem_File, ListItem_Folder),
    listItem,
  )
import qualified Neuron.Plugin.Plugins.Tags as Tags
import Neuron.Plugin.Type (Plugin (..))
import Neuron.Zettelkasten.Connection (Connection (Folgezettel, OrdinaryConnection), ContextualConnection)
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.ID (ZettelID (ZettelID))
import Neuron.Zettelkasten.Resolver (ZIDRef (..))
import qualified Neuron.Zettelkasten.Resolver as R
import Neuron.Zettelkasten.Zettel
import Reflex.Dom.Core
import Relude hiding (trace, traceShow, traceShowId)
import qualified System.Directory.Contents as DC
import System.FilePath (takeDirectory, takeFileName)
import qualified Text.Pandoc.Definition as P

-- Directory zettels using this plugin are associated with a `Tag` that
-- corresponds to the directory contents.
plugin :: Plugin DirZettelVal
plugin =
  def
    { _plugin_afterZettelRead = injectDirectoryZettels,
      _plugin_afterZettelParse = const $ bimap addTagAndQuery addTagAndQuery,
      _plugin_graphConnections = queryConnections,
      _plugin_renderPanel = const renderPanel
    }

queryConnections ::
  forall m.
  MonadReader [Zettel] m =>
  Zettel ->
  m [(ContextualConnection, Zettel)]
queryConnections Zettel {..} = do
  zs <- ask
  case DMap.lookup PluginZettelData_DirTree zettelPluginData of
    Just (Identity (DirTreeZettel_Dir DirZettel {..})) -> do
      pure $ getChildZettels zs _dirZettel_childrenTag
    _ -> pure mempty

routePluginData :: ZettelGraph -> DirTreeZettel -> DirZettelVal
routePluginData g = \case
  DirTreeZettel_Dir DirZettel {..} ->
    let children = getChildZettels (G.getZettels g) _dirZettel_childrenTag
        mparent = flip G.getZettel g =<< _dirZettel_dirParent
     in DirZettelVal children mparent
  DirTreeZettel_Regular _tags mparent' ->
    let mparent = flip G.getZettel g =<< mparent'
     in DirZettelVal mempty mparent -- TODO

getChildZettels :: [Zettel] -> Tag -> [(ContextualConnection, Zettel)]
getChildZettels zs t =
  let childrenQuery = mkDefaultTagQuery $ one $ Tag.mkTagPatternFromTag t
      ctx = one $ P.Plain $ one $ P.Emph $ one $ P.Str "Directory Zettel"
      -- Child zettels are folgezettel, with the exception of the children of
      -- root dir zettel. Why? Because we don't want one huge cluster glued
      -- together by the index.
      conn = if t == Tag.constructTag (one rootTag) then OrdinaryConnection else Folgezettel
   in ((conn, ctx),) <$> Tags.zettelsByTag getZettelDirTags zs childrenQuery

getZettelDirTags :: ZettelT c -> Set Tag
getZettelDirTags Zettel {..} =
  fromMaybe Set.empty $ do
    DMap.lookup PluginZettelData_DirTree zettelPluginData >>= \case
      Identity (DirTreeZettel_Dir DirZettel {..}) ->
        pure _dirZettel_tags
      Identity (DirTreeZettel_Regular tags _mparent) ->
        pure tags

renderPanel :: (DomBuilder t m, PostBuild t m) => DirZettelVal -> NeuronWebT t m ()
renderPanel DirZettelVal {..} = do
  when (isJust dirZettelValParent || not (null dirZettelValChildren)) $ do
    elClass "nav" "ui attached segment dirfolge" $ do
      divClass "ui list" $ do
        whenJust dirZettelValParent $ \parZ ->
          listItem ListItem_Folder $
            Q.renderZettelLink (Just $ elClass "i" "level up alternate icon" blank) Nothing Nothing parZ
        forM_ dirZettelValChildren $ \((conn, _ctx), cz) ->
          listItem (bool ListItem_File ListItem_Folder $ isDirectoryZettel cz) $
            Q.renderZettelLink Nothing (Just conn) Nothing cz
      elAttr "a" ("href" =: pluginDoc <> "title" =: "What is this section about?") $ elClass "i" "question circle outline icon" blank
  where
    pluginDoc = "https://neuron.zettel.page/dirtree.html"

addTagAndQuery :: forall c. ZettelT c -> ZettelT c
addTagAndQuery z =
  case runIdentity <$> DMap.lookup PluginZettelData_DirTree (zettelPluginData z) of
    Just (DirTreeZettel_Dir DirZettel {}) ->
      -- This is a directory zettel; nothing to modify.
      z
    _ ->
      -- Regular zettel; add the tag based on path.
      let tags = maybe Set.empty Set.singleton $ parentDirTag $ zettelPath z
          mparent = do
            guard $ zettelID z /= indexZettelID
            pure $ parentZettelIDFromPath (zettelPath z)
       in z
            { -- TODO ??
              zettelPluginData = DMap.insert PluginZettelData_DirTree (Identity $ DirTreeZettel_Regular tags mparent) (zettelPluginData z)
            }

parentZettelIDFromPath :: FilePath -> ZettelID
parentZettelIDFromPath p =
  let parDirName = takeFileName (takeDirectory p)
      parDirZettelId = if parDirName == "." then indexZettelID else ZettelID (toText parDirName)
   in parDirZettelId

injectDirectoryZettels :: MonadState (Map ZettelID ZIDRef) m => DC.DirTree FilePath -> m ()
injectDirectoryZettels = \case
  DC.DirTree_Dir absPath contents -> do
    let dirName = takeFileName absPath
        dirZettelId = ZettelID $ if dirName == "." then indexZettelName else toText dirName
        mparent = do
          guard $ absPath /= "."
          pure $ parentZettelIDFromPath absPath
        mkPluginData tags =
          DMap.singleton PluginZettelData_DirTree $ Identity $ DirTreeZettel_Dir $ DirZettel tags (tagFromPath absPath) mparent
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

indexZettelID :: ZettelID
indexZettelID = ZettelID indexZettelName

rootTag :: TagNode
rootTag = TagNode "root"

isDirectoryZettel :: ZettelT content -> Bool
isDirectoryZettel (zettelPluginData -> pluginData) =
  case DMap.lookup PluginZettelData_DirTree pluginData of
    Just (Identity (DirTreeZettel_Dir _)) -> True
    _ -> False
