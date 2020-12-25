{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- TODO: plugin system
module Neuron.Plugin.DirectoryFolgezettel where

import Data.TagTree (Tag (Tag))
import qualified Data.Text as T
import Neuron.Zettelkasten.Zettel (ZettelT (zettelTags))
import Relude
import System.FilePath (takeDirectory)

-- | Add a hierarchical tag based on the directory the zettel is in.
--
-- For example, the zettel @foo/bar/qux.md@ gets tagged with @root/foo/bar@.
-- And @note.md@ gets tagged with @root@. @index.md@, however, will not be
-- tagged.
postZettelParseHook :: HasCallStack => FilePath -> ZettelT c -> ZettelT c
postZettelParseHook fn z =
  if Tag "dirfolge" `elem` zettelTags z
    then z
    else z {zettelTags = zettelTags z <> maybeToList (parentDirTag fn)}

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