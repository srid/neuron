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
  if Tag "dirfolge" `elem` zettelTags z
    then z
    else z {zettelTags = zettelTags z <> maybeToList (parentDirTag $ zettelPath z)}

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
    let dirZettelId = ZettelID $ toText $ if dirName == "." then "index" else dirName
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
        R.addZettel ("<dirfolge:autogen:" <> absPath <> ">") dirZettelId $ do
          let header = "# " <> toText (takeFileName absPath) <> "/"
          pure $ header <> directoryZettelContents absPath
    forM_ (Map.toList contents) $ \(_, ct) ->
      injectDirectoryFolgezettels ct
  _ ->
    -- We ignore symlinks, and paths configured to be excluded.
    pure ()
  where
    directoryZettelContents absPath =
      let thisTag = case takeDirectory absPath of
            "." -> "index"
            x -> unTag $ tagFromPath x
          inlineTags = "#dirfolge #" <> thisTag
          -- TODO: should just do arbitrary zettel `content` type? (`ZettelT DirZettel`)
          md = "## Directory contents\n\n[[[z:zettels?tag=" <> unTag (tagFromPath absPath) <> "]]]\n"
       in "\n\n" <> md <> "\n" <> inlineTags
