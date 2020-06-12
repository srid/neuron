{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.CLI.New
  ( newZettelFile,
  )
where

import qualified Data.Set as Set
import Data.Some
import Data.Text (strip)
import qualified Data.Text as T
import qualified Data.YAML as YAML
import Development.Shake (Action, CmdOption(..), cmd)
import Neuron.CLI.Types hiding (cmd)
import Neuron.Web.Generate as Gen
import Neuron.Zettelkasten.ID (ZettelID, zettelIDSourceFileName)
import qualified Neuron.Zettelkasten.ID.Scheme as IDScheme
import Neuron.Zettelkasten.Zettel (zettelID)
import Options.Applicative hiding (command)
import Relude
import qualified Rib
import System.FilePath
import qualified System.Posix.Env as Env

-- | Create a new zettel file and open it in editor if requested
--
-- As well as print the path to the created file.
newZettelFile :: NewCommand -> Action ()
newZettelFile NewCommand {..} = do
  (_, zettels, _) <- Gen.loadZettelkasten
  mzid <- withSome idScheme $ \scheme -> do
    val <- liftIO $ IDScheme.genVal scheme
    pure $
      IDScheme.nextAvailableZettelID
        (Set.fromList $ fmap (either zettelID zettelID) zettels)
        val
        scheme
  case mzid of
    Left e -> die $ show e
    Right zid -> do
      (notesDir, filename) <- zettelPath zid
      liftIO $ do
        fileAction :: (FilePath, FilePath) -> IO () <-
          bool (pure showAction) mkEditActionFromEnv edit
        writeFileText (notesDir </> filename) $
          T.intercalate
            "\n"
            [ "---",
              "title: " <> T.strip (decodeUtf8 (YAML.encode1 title)),
              "date: " <> T.strip (decodeUtf8 (YAML.encode1 day)),
              "---",
              "\n"
            ]
        fileAction (notesDir, filename)
  where
    mkEditActionFromEnv :: IO ((FilePath, FilePath) -> IO ())
    mkEditActionFromEnv =
      getEnvNonEmpty "EDITOR" >>= \case
        Nothing ->
          die "\n-e option can only be used with EDITOR environment variable set"
        Just editor ->
          pure $ editAction editor
    editAction editor (notesDir, filename) = do
      -- Show it first in case the editor launch fails
      showAction (notesDir, filename)
      cmd (Cwd notesDir) Shell editor [filename] :: IO ()
    showAction =
      putStrLn . uncurry (</>)
    getEnvNonEmpty name =
      Env.getEnv name >>= \case
        Nothing -> pure Nothing
        Just (toString . strip . toText -> v) ->
          if null v then pure Nothing else pure (Just v)

zettelPath :: ZettelID -> Action (FilePath, FilePath) 
zettelPath zid = do
  notesDir <- Rib.ribInputDir
  pure $ (notesDir , zettelIDSourceFileName zid)
