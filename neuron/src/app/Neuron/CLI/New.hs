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
import Data.Time
import qualified Data.YAML as YAML
import Development.Shake (Action)
import Neuron.CLI.Types
import Neuron.Config.Type (Config (..))
import Neuron.Reader.Type (ZettelFormat (..))
import Neuron.Web.Generate as Gen
import Neuron.Zettelkasten.ID (zettelIDSourceFileName)
import qualified Neuron.Zettelkasten.ID.Scheme as IDScheme
import Neuron.Zettelkasten.Zettel (zettelID)
import Neuron.Zettelkasten.Zettel.Meta ()
import Options.Applicative
import Relude
import qualified Rib
import System.Directory (setCurrentDirectory)
import System.FilePath
import qualified System.Posix.Env as Env
import System.Posix.Process

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
      notesDir <- Rib.ribInputDir
      defaultFormat <- maybe (fail "Cannot pick default format: neuron configuration does not specify any formats") pure $ do
        (_, fmt) <- viaNonEmpty head $ formats config
        readMaybe $ toString fmt
      let zettelFormat = fromMaybe defaultFormat format
          zettelFile = zettelIDSourceFileName zid zettelFormat
      liftIO $ do
        fileAction :: FilePath -> FilePath -> IO () <-
          bool (pure showAction) mkEditActionFromEnv edit
        writeFileText (notesDir </> zettelFile) $ defaultZettelContent zettelFormat day title
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

-- TODO use configurable template files?
defaultZettelContent :: ZettelFormat -> Day -> Maybe Text -> Text
defaultZettelContent format day mtitle = case format of
  ZettelFormat_Markdown ->
    T.intercalate
      "\n"
      [ "---",
        "date: " <> date,
        "---",
        "",
        "# " <> title,
        "\n"
      ]
  ZettelFormat_Org ->
    T.intercalate
      "\n"
      [ "* " <> title,
        "    :PROPERTIES:",
        "    :Date: " <> date,
        "    :END:",
        "\n"
      ]
  where
    date = T.strip (decodeUtf8 (YAML.encode1 day))
    defaultTitleName = "Zettel created on " <> date
    title = maybe defaultTitleName T.strip mtitle
