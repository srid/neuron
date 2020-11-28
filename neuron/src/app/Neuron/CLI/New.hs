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
import Data.Some (withSome)
import Data.Text (strip)
import qualified Data.Text as T
import Data.Time.DateMayTime (DateMayTime, formatDateMayTime)
import Development.Shake (Action)
import Neuron.CLI.Types (NewCommand (..))
import Neuron.Config.Type (Config (..), getZettelFormats)
import Neuron.Reader.Type (ZettelFormat (..))
import Neuron.Web.Generate as Gen (loadZettelkasten)
import Neuron.Zettelkasten.ID (zettelIDSourceFileName)
import qualified Neuron.Zettelkasten.ID.Scheme as IDScheme
import Neuron.Zettelkasten.Zettel (zettelID)
import Relude
import Rib.Shake (ribInputDir)
import System.Directory (setCurrentDirectory)
import System.FilePath ((</>))
import qualified System.Posix.Env as Env
import System.Posix.Process (executeFile)

-- | Create a new zettel file and open it in editor if requested
--
-- As well as print the path to the created file.
newZettelFile :: NewCommand -> Config -> Action ()
newZettelFile NewCommand {..} config = do
  (_, zettels, _) <- Gen.loadZettelkasten config
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
      notesDir <- ribInputDir
      defaultFormat <- head <$> getZettelFormats config
      let zettelFormat = fromMaybe defaultFormat format
          zettelFile = zettelIDSourceFileName zid zettelFormat
      liftIO $ do
        fileAction :: FilePath -> FilePath -> IO () <-
          bool (pure showAction) mkEditActionFromEnv edit
        writeFileText (notesDir </> zettelFile) $ defaultZettelContent zettelFormat date
        fileAction notesDir zettelFile
  where
    mkEditActionFromEnv :: IO (FilePath -> FilePath -> IO ())
    mkEditActionFromEnv =
      getEnvNonEmpty "EDITOR" >>= \case
        Nothing ->
          die "\n-e option can only be used with EDITOR environment variable set"
        Just editorCli ->
          pure $ editAction editorCli
    editAction editorCli notesDir zettelFile = do
      -- Show the path first, in case the editor launch fails
      showAction notesDir zettelFile
      setCurrentDirectory notesDir
      executeShellCommand $ editorCli <> " " <> zettelFile
    showAction notesDir zettelFile =
      putStrLn $ notesDir </> zettelFile
    -- Like `executeFile` but takes a shell command.
    executeShellCommand cmd =
      executeFile "bash" True ["-c", cmd] Nothing
    getEnvNonEmpty name =
      Env.getEnv name >>= \case
        Nothing -> pure Nothing
        Just (toString . strip . toText -> v) ->
          if null v then pure Nothing else pure (Just v)

defaultZettelContent :: ZettelFormat -> DateMayTime -> Text
defaultZettelContent format (formatDateMayTime -> date) = case format of
  ZettelFormat_Markdown ->
    T.intercalate
      "\n"
      [ "---",
        "date: " <> date,
        "---",
        ""
      ]
  ZettelFormat_Org ->
    T.intercalate
      "\n"
      [ "* " <> "Untitled", -- Leaving a title here, only because org seems to require one
        "    :PROPERTIES:",
        "    :Date: " <> date,
        "    :END:",
        "\n"
      ]
