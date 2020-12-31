{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module System.Directory.Contents.Extra where

import qualified Data.Map.Strict as Map
import Relude
import System.Directory (copyFile, createDirectoryIfMissing)
import qualified System.Directory.Contents as DC
import System.FilePath ((</>))
import System.Posix (fileExist, getFileStatus, modificationTime)

-- | Copy the given directory tree from @src@ as base directory, to @dest@
rsyncDir :: FilePath -> FilePath -> DC.DirTree FilePath -> IO ()
rsyncDir src dest = \case
  DC.DirTree_File fp _ -> do
    let (a, b) = (src </>) &&& (dest </>) $ fp
    aT <- modificationTime <$> getFileStatus a
    -- TODO: if a file gets deleted, we must remove it.
    mBT <- do
      fileExist b >>= \case
        True -> do
          bT <- modificationTime <$> getFileStatus b
          pure $ Just bT
        False ->
          pure Nothing
    when (maybe True (aT >) mBT) $ do
      putStrLn $ "[cp] " <> fp
      copyFile a b
  DC.DirTree_Symlink {} ->
    pure ()
  DC.DirTree_Dir dp children -> do
    createDirectoryIfMissing False (dest </> dp)
    forM_ (Map.elems children) $ \childTree -> do
      rsyncDir src dest childTree
