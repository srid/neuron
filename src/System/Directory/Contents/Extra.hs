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

import Colog (WithLog, log)
import qualified Data.Map.Strict as Map
import Neuron.CLI.Logging
import Relude
import System.Directory (copyFile, createDirectoryIfMissing)
import qualified System.Directory.Contents as DC
import System.FilePath ((</>))
import System.PosixCompat (fileExist, getFileStatus, modificationTime)

-- | Copy the given directory tree from @src@ as base directory, to @dest@
rsyncDir :: (MonadIO m, WithLog env Message m) => FilePath -> FilePath -> DC.DirTree FilePath -> m Int
rsyncDir src dest = \case
  DC.DirTree_File fp _ -> do
    let (a, b) = (src </>) &&& (dest </>) $ fp
    aT <- liftIO $ modificationTime <$> getFileStatus a
    -- TODO: if a file gets deleted, we must remove it.
    mBT <- liftIO $ do
      fileExist b >>= \case
        True -> do
          bT <- modificationTime <$> getFileStatus b
          pure $ Just bT
        False ->
          pure Nothing
    if maybe True (aT >) mBT
      then do
        log I $ toText $ "+ " <> fp
        liftIO $ copyFile a b
        pure 1
      else pure 0
  DC.DirTree_Symlink {} ->
    pure 0
  DC.DirTree_Dir dp children -> do
    liftIO $ createDirectoryIfMissing False (dest </> dp)
    fmap sum $
      forM (Map.elems children) $ \childTree -> do
        rsyncDir src dest childTree
