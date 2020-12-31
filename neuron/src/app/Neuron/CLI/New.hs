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
import Neuron.CLI.Types (MonadApp, NewCommand (..), getNotesDir)
import Neuron.Config.Type (Config (..))
import Neuron.Gen as Gen (loadZettelkasten)
import qualified Neuron.Web.Cache.Type as Cache
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.ID (zettelIDSourceFileName)
import qualified Neuron.Zettelkasten.ID.Scheme as IDScheme
import Neuron.Zettelkasten.Zettel (zettelID)
import Relude
import System.Directory (setCurrentDirectory)
import System.FilePath ((</>))
import qualified System.Posix.Env as Env
import System.Posix.Process (executeFile)

-- | Create a new zettel file and open it in editor if requested
--
-- As well as print the path to the created file.
newZettelFile :: (MonadIO m, MonadApp m) => NewCommand -> Config -> m ()
newZettelFile NewCommand {..} config = do
  (g, _, _) <- Gen.loadZettelkasten config
  mzid <- withSome idScheme $ \scheme -> do
    val <- liftIO $ IDScheme.genVal scheme
    pure $
      IDScheme.nextAvailableZettelID
        (Set.fromList $ fmap zettelID $ G.getZettels $ Cache._neuronCache_graph g)
        val
        scheme
  case mzid of
    Left e -> die $ show e
    Right zid -> do
      notesDir <- getNotesDir
      let zettelFile = zettelIDSourceFileName zid
      liftIO $ do
        fileAction :: FilePath -> FilePath -> IO () <-
          bool (pure showAction) mkEditActionFromEnv edit
        writeFileText (notesDir </> zettelFile) $ defaultZettelContent date
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

defaultZettelContent :: DateMayTime -> Text
defaultZettelContent (formatDateMayTime -> date) =
  T.intercalate
    "\n"
    [ "---",
      "date: " <> date,
      "---",
      ""
    ]
