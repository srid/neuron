{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- TODO: plugin system
module Neuron.Plugin.DirectoryFolgezettel where

import qualified Data.Map.Strict as Map
import Data.TagTree (Tag (..))
import qualified Data.Text as T
import Development.Shake (Action)
import Neuron.Zettelkasten.ID (ZettelID (ZettelID))
import Neuron.Zettelkasten.Resolver (ZIDRef (..))
import qualified Neuron.Zettelkasten.Resolver as R
import Neuron.Zettelkasten.Zettel (ZettelT (zettelPath, zettelTags))
import Relude
import qualified System.Directory.Contents as DC
import System.FilePath (takeDirectory, takeFileName)

-- | Add a hierarchical tag based on the directory the zettel is in.
--
-- For example, the zettel @foo/bar/qux.md@ gets tagged with @root/foo/bar@.
-- And @note.md@ gets tagged with @root@. @index.md@, however, will not be
-- tagged.
postZettelParseHook :: forall c. HasCallStack => ZettelT c -> ZettelT c
postZettelParseHook z =
  -- REVIEW: weird behaviour? ala. ./2020.md vs ./Memory/2020/ -- not totally bad though.
  z
    { zettelTags = zettelTags z <> maybeToList (parentDirTag $ zettelPath z)
    }

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

injectDirectoryFolgezettels :: DC.DirTree FilePath -> StateT (Map ZettelID ZIDRef) Action ()
injectDirectoryFolgezettels = \case
  DC.DirTree_File _relPath _ -> do
    pure ()
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
      gets (Map.lookup dirZettelId) >>= \case
        Just ref -> do
          case ref of
            ZIDRef_Available p s -> do
              let s' = s <> directoryZettelContents absPath
              modify $ Map.update (const $ Just $ ZIDRef_Available p s') dirZettelId
            ZIDRef_Ambiguous {} ->
              -- TODO: What do do here?
              pure ()
        Nothing -> do
          R.addZettel (absPath <> ".md.dirfolge") dirZettelId $ do
            let header = "# " <> toText (takeFileName absPath) <> "/"
            pure $ header <> directoryZettelContents absPath
    forM_ (Map.toList contents) $ \(_, ct) ->
      injectDirectoryFolgezettels ct
  _ ->
    -- We ignore symlinks, and paths configured to be excluded.
    pure ()
  where
    directoryZettelContents absPath =
      let dirName = takeFileName absPath
       in "\n\n:::{.ui .deemphasized .small .segment}\nNotes under the *" <> toText dirName <> "/* directory:\n\n[[[z:zettels?tag=" <> unTag (tagFromPath absPath) <> "]]]\n:::\n"
