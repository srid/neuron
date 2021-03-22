{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
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

module Neuron.Plugin.Plugins.DirTree
  ( plugin,
    routePluginData,
    renderPanel,
  )
where

import qualified Data.Dependent.Map as DMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.TagTree (Tag, TagNode (..), mkDefaultTagQuery)
import qualified Data.TagTree as Tag
import qualified Data.Text as T
import Neuron.Frontend.Route (NeuronWebT)
import Neuron.Frontend.Route.Data.Types (DirZettelVal (..))
import Neuron.Frontend.Widget
  ( ListItem (ListItem_File, ListItem_Folder),
    listItem,
  )
import Neuron.Markdown (lookupZettelMeta)
import qualified Neuron.Plugin.Plugins.Links as Links
import Neuron.Plugin.Plugins.Tags (appendTags)
import qualified Neuron.Plugin.Plugins.Tags as Tags
import Neuron.Plugin.Type (Plugin (..))
import Neuron.Zettelkasten.Connection (Connection (Folgezettel, OrdinaryConnection), ContextualConnection)
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.ID (ZettelID (ZettelID, unZettelID), indexZid)
import Neuron.Zettelkasten.Resolver (ZIDRef (..))
import qualified Neuron.Zettelkasten.Resolver as R
import Neuron.Zettelkasten.Zettel
import Reflex.Dom.Core
import Relude
import qualified System.Directory.Contents as DC
import System.FilePath (takeDirectory, takeFileName, splitDirectories)
import qualified Text.Pandoc.Definition as P

-- Directory zettels using this plugin are associated with a `Tag` that
-- corresponds to the directory contents.
plugin :: Plugin DirZettelVal
plugin =
  def
    { _plugin_afterZettelRead = injectDirectoryZettels,
      _plugin_afterZettelParse = bimap afterZettelParse afterZettelParse,
      _plugin_graphConnections = queryConnections,
      _plugin_renderPanel = const renderPanel
    }

queryConnections ::
  forall m.
  MonadReader [Zettel] m =>
  Zettel ->
  m [(ContextualConnection, Zettel)]
queryConnections z = do
  zs <- ask
  case lookupPluginData DirTree z of
    Just (DirZettel _ _ (Just childTag) _) -> do
      pure $ getChildZettels zs childTag
    _ -> pure mempty

routePluginData :: ZettelGraph -> DirZettel -> DirZettelVal
routePluginData g (DirZettel _ parent mChildTag mMeta) =
  let children = maybe mempty (getChildZettels (G.getZettels g)) mChildTag
      mparent = flip G.getZettel g =<< parent
   in DirZettelVal children mparent (fromMaybe def mMeta)

getChildZettels :: [Zettel] -> Tag -> [(ContextualConnection, Zettel)]
getChildZettels zs t =
  let childrenQuery = mkDefaultTagQuery $ one $ Tag.mkTagPatternFromTag t
      ctx = one $ P.Plain $ one $ P.Emph $ one $ P.Str "Parent directory zettel"
      -- Child zettels are folgezettel, with the exception of the children of
      -- root dir zettel. Why? Because we don't want one huge cluster glued
      -- together by the index.
      conn = if t == Tag.constructTag (one rootTag) then OrdinaryConnection else Folgezettel
   in ((conn, ctx),) <$> Tags.zettelsByTag getZettelDirTags zs childrenQuery

getZettelDirTags :: ZettelT c -> Set Tag
getZettelDirTags z =
  fromMaybe Set.empty $ do
    _dirZettel_tags <$> lookupPluginData DirTree z

renderPanel :: (DomBuilder t m, PostBuild t m) => DirZettelVal -> NeuronWebT t m ()
renderPanel DirZettelVal {..} = do
  when (dirtreemetaDisplay dirZettelValMeta) $ do
    unless (null dirZettelValChildren) $ do
      elClass "nav" "ui attached segment dirfolge" $ do
        divClass "ui list" $ do
          whenJust dirZettelValParent $ \parZ ->
            listItem ListItem_Folder $
              Links.renderZettelLink (Just $ elClass "i" "level up alternate icon" blank) Nothing Nothing parZ
          forM_ dirZettelValChildren $ \((conn, _ctx), cz) ->
            listItem (bool ListItem_File ListItem_Folder $ isDirectoryZettel cz) $
              Links.renderZettelLink Nothing (Just conn) Nothing cz
        elAttr "a" ("href" =: pluginDoc <> "title" =: "What is this section about?") $ elClass "i" "question circle outline icon" blank
  where
    pluginDoc = "https://neuron.zettel.page/dirtree"

afterZettelParse :: forall c. ZettelT c -> ZettelT c
afterZettelParse z =
  case lookupPluginData DirTree z of
    Just (DirZettel tags mparent (Just childTag) Nothing) ->
      let pluginData = DirZettel tags mparent (Just childTag) getMeta
       in setDirTreePluginData pluginData
    Just (DirZettel _ _ (Just _) _) ->
      -- This is a directory zettel; nothing to modify.
      z
    _
      | zettelID z == indexZid ->
        z
    _ ->
      -- Regular zettel; add the tag based on path.
      let tags = maybe Set.empty Set.singleton $ parentDirTag $ zettelPath z
          mparent = do
            guard $ zettelID z /= indexZid
            pure $ parentZettelIDFromPath (zettelPath z)
          pluginData = DirZettel tags mparent Nothing getMeta
       in setDirTreePluginData pluginData
  where
    setDirTreePluginData pluginData =
      -- Add plugin-specific tags to metadata, so the user may query based on them.
      setPluginData DirTree pluginData $ appendTags (_dirZettel_tags pluginData) z
    getMeta =
      lookupZettelMeta @DirTreeMeta "dirtree" (zettelMeta z)

parentZettelIDFromPath :: FilePath -> ZettelID
parentZettelIDFromPath p =
  let parDirName = takeFileName (takeDirectory p)
   in if parDirName == "." then indexZid else ZettelID (toText parDirName)

injectDirectoryZettels :: MonadState (Map ZettelID ZIDRef) m => DC.DirTree FilePath -> m ()
injectDirectoryZettels = \case
  DC.DirTree_Dir absPath contents -> do
    let dirName = takeFileName absPath
        dirZettelId = ZettelID $ if dirName == "." then unZettelID indexZid else toText dirName
        mparent = do
          guard $ absPath /= "."
          pure $ parentZettelIDFromPath absPath
        meta = Nothing -- if $dirname.md exists that will be handled by `afterZettelParse`
        mkPluginData tags =
          DMap.singleton DirTree $
            Identity $ DirZettel tags mparent (Just $ tagFromPath absPath) meta
    gets (Map.lookup dirZettelId) >>= \case
      Just (ZIDRef_Available p s pluginDataPrev) -> do
        -- A zettel with this directory name was already registered. Deal with it.
        case runIdentity <$> DMap.lookup DirTree pluginDataPrev of
          Just _ -> do
            -- A *directory* zettel of this name was already added.
            -- Ambiguous directories disallowed! For eg., you can't have
            -- Foo/Qux and Bar/Qux.
            R.markAmbiguous dirZettelId $ absPath :| [p]
          Nothing -> do
            -- A file zettel (DIRNAME.md) already exists on disk. Merge with it.
            let pluginData =
                  mkPluginData $
                    Set.fromList $
                      catMaybes
                        [ guard (dirZettelId /= indexZid) >> parentDirTag absPath,
                          parentDirTag p
                        ]
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
parentDirTag relPath =
  let paths = splitDirectories relPath
   in case paths of
      ["."]-> Nothing
      [".", "index.md"] -> Nothing
      _ -> Just $ tagFromPath $ takeDirectory relPath

tagFromPath :: HasCallStack => FilePath -> Tag
tagFromPath relPath = 
  let paths = splitDirectories relPath
  in case paths of
    ["."] ->
      -- Root file
      Tag.constructTag (rootTag :| [])
    (".":dirPaths) ->
      let tagNodes = TagNode <$> map (\p -> T.replace " " "-" $ toText p) dirPaths
      in Tag.constructTag $ rootTag :| tagNodes
    _ ->
      error $ "Invalid relPath passed to parseZettel: " <> toText relPath

rootTag :: TagNode
rootTag = TagNode "root"

isDirectoryZettel :: ZettelT c -> Bool
isDirectoryZettel z =
  case lookupPluginData DirTree z of
    Just (DirZettel _ _ (Just _childTag) _) ->
      True
    _ ->
      False
