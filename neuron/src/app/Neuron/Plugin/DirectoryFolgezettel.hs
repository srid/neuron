{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Plugin.DirectoryFolgezettel (plugin) where

import Data.Default (Default (def))
import qualified Data.Dependent.Map as DMap
import qualified Data.Map.Strict as Map
import Data.Some (Some (Some))
import Data.TagTree (Tag (Tag), mkTagPatternFromTag)
import qualified Data.Text as T
import Neuron.Plugin.Type (Plugin (Plugin))
import Neuron.Zettelkasten.Connection (Connection (Folgezettel))
import Neuron.Zettelkasten.ID (ZettelID (ZettelID))
import Neuron.Zettelkasten.Resolver (ZIDRef (..))
import qualified Neuron.Zettelkasten.Resolver as R
import Neuron.Zettelkasten.Zettel (ZettelPluginData (..), ZettelQuery (..), ZettelT (..))
import Relude
import qualified System.Directory.Contents as DC
import System.FilePath (takeDirectory, takeFileName)

plugin :: Plugin
plugin =
  Plugin "dirfolge" injectDirectoryZettels (bimap addTagAndQuery addTagAndQuery)

addTagAndQuery :: forall c. HasCallStack => ZettelT c -> ZettelT c
addTagAndQuery z =
  z
    { zettelTags =
        zettelTags z <> maybeToList (parentDirTag $ zettelPath z),
      -- Add the tag query for building graph connections.
      zettelQueries =
        -- TODO: Surrounding context
        zettelQueries z <> fmap (,mempty) (maybeToList tagQuery)
    }
  where
    tagQuery :: Maybe (Some ZettelQuery)
    tagQuery = do
      t <- join $ join $ DMap.lookup ZettelPluginData_DirectoryZettel (zettelPluginData z)
      pure $ Some $ ZettelQuery_ZettelsByTag [mkTagPatternFromTag t] Folgezettel def

injectDirectoryZettels :: MonadState (Map ZettelID ZIDRef) m => DC.DirTree FilePath -> m ()
injectDirectoryZettels = \case
  DC.DirTree_Dir absPath contents -> do
    let dirName = takeFileName absPath
        dirZettelId = ZettelID $ toText $ if dirName == "." then "index" else dirName
    -- Don't create folgezettel from index zettel. Why?
    -- - To avoid surprise when legacy notebooks with innuermous top level
    -- zettels use this feature.
    -- - Facilitate multiple clusters that don't "stick" because of index-folgezettel.
    -- If the user wants to make index branch to these top-level zettels,
    -- they can add `[[[z:zettels?tag=index]]]` to do that.
    unless (dirZettelId == ZettelID "index") $ do
      let dirTag = tagFromPath absPath
      gets (Map.lookup dirZettelId) >>= \case
        Just ref -> do
          case ref of
            ZIDRef_Available p s pluginData -> do
              -- If a zettel with the same name as the directory already exists,
              -- treat that zettel as the directory zettel, by adding this
              -- plugin's data to it.
              let pluginData' = DMap.insert ZettelPluginData_DirectoryZettel (Just $ Just dirTag) pluginData
              modify $ Map.update (const $ Just $ ZIDRef_Available p s pluginData') dirZettelId
            ZIDRef_Ambiguous {} ->
              -- TODO: What to do here?
              pure ()
        Nothing -> do
          -- Inject a new zettel corresponding to this directory.
          let pluginData = DMap.singleton ZettelPluginData_DirectoryZettel (Just $ Just dirTag)
          R.addZettel absPath dirZettelId pluginData $ do
            -- Set an appropriate title (same as directory name)
            pure $ "# " <> toText (takeFileName absPath) <> "/"
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
