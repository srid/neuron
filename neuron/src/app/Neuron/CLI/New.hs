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
import Development.Shake (Action)
import Neuron.CLI.Types
import Neuron.Web.Generate as Gen
import Neuron.Zettelkasten.ID (ZettelID, zettelIDSourceFileName)
import qualified Neuron.Zettelkasten.ID.Scheme as IDScheme
import Neuron.Zettelkasten.Zettel (zettelID)
import Options.Applicative
import Relude
import qualified Rib
import System.FilePath
import qualified System.Posix.Env as Env
import System.Posix.Process

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
      path <- zettelPath zid
      liftIO $ do
        fileAction :: FilePath -> IO () <-
          bool (pure showAction) mkEditActionFromEnv edit
        writeFileText path $
          T.intercalate
            "\n"
            [ "---",
              "title: " <> T.strip (decodeUtf8 (YAML.encode1 title)),
              "date: " <> T.strip (decodeUtf8 (YAML.encode1 day)),
              "---",
              "\n"
            ]
        fileAction path
  where
    mkEditActionFromEnv :: IO (FilePath -> IO ())
    mkEditActionFromEnv =
      getEnvNonEmpty "EDITOR" >>= \case
        Nothing ->
          die "\n-e option can only be used with EDITOR environment variable set"
        Just editor ->
          pure $ editAction editor
    editAction editor path = do
      -- Show it first in case the editor launch fails
      showAction path
      executeFile "bash" True ["-c", editor ++ ' ' : path ] Nothing
    showAction =
      putStrLn
    getEnvNonEmpty name =
      Env.getEnv name >>= \case
        Nothing -> pure Nothing
        Just (toString . strip . toText -> v) ->
          if null v then pure Nothing else pure (Just v)

zettelPath :: ZettelID -> Action FilePath
zettelPath zid = do
  notesDir <- Rib.ribInputDir
  pure $ notesDir </> zettelIDSourceFileName zid
