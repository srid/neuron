{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- TODO: plugin system
module Neuron.Plugin.DirectoryFolgezettel where

import Data.TagTree (Tag (Tag))
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
  z {zettelTags = zettelTags z <> [parentDirTag fn]}
  where
    parentDirTag :: HasCallStack => FilePath -> Tag
    parentDirTag = \case
      "./index.md" ->
        Tag "undefined" -- should be Nothing
      (takeDirectory -> ".") ->
        -- Root file
        Tag indexZettelName
      (takeDirectory -> '.' : '/' : dirPath) ->
        Tag $ indexZettelName <> "/" <> toText dirPath
      relPath ->
        error $ "Invalid relPath passed to parseZettel: " <> toText relPath
      where
        indexZettelName = "index"
