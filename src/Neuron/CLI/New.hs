{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.CLI.New
  ( newZettelFile
  )
where

import Development.Shake (Action)
import Neuron.CLI.Types
import qualified Neuron.Zettelkasten.ID as Z
import Options.Applicative
import Relude
import qualified Rib
import System.Directory
import System.FilePath
import qualified System.Posix.Env as Env
import System.Posix.Process

-- | Create a new zettel file and open it in editor if requested
--
-- As well as print the path to the created file.
newZettelFile :: NewCommand -> Action ()
newZettelFile NewCommand {..} = do
  zId <- Z.zettelNextIdForToday
  let zettelFileName = toString $ Z.zettelIDSourceFileName zId
  inputDir <- Rib.ribInputDir
  let srcPath = inputDir </> zettelFileName
  liftIO $
    doesFileExist srcPath >>= \case
      True ->
        fail $ "File already exists: " <> show srcPath
      False -> do
        writeFile srcPath $ "---\ntitle: " <> toString title <> "\n---\n\n"
        putStrLn srcPath
        when edit $ do
          getEnvNonEmpty "EDITOR" >>= \case
            Nothing -> do
              die "\nCan't open file; you must set the EDITOR environment variable"
            Just editor -> do
              executeFile editor True [srcPath] Nothing
  where
    getEnvNonEmpty name =
      Env.getEnv name >>= \case
        Nothing -> pure Nothing
        Just "" -> pure Nothing
        Just v -> pure $ Just v
