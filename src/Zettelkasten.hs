{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Self.Zettelkasten
  ( generateSite,
    commandParser,
    run,
    runWith,
    newZettelFile,
  )
where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time
import Development.Shake (Action)
import Options.Applicative
import Path
import Path.IO
import Relude
import qualified Rib
import qualified Rib.App
import qualified Self.Zettelkasten.Graph as Z
import qualified Self.Zettelkasten.Route as Z
import qualified Self.Zettelkasten.Store as Z
import System.Directory (listDirectory)
import qualified System.FilePattern as FP
import Text.Printf

data Command
  = -- | Create a new zettel file
    New Text
  | Rib Rib.App.Command
  deriving (Eq, Show)

commandParser :: Parser Command
commandParser =
  hsubparser $
    mconcat
      [ command "new" $ info newCommand $ progDesc "Create a new zettel",
        command "rib" $ fmap Rib $ info Rib.App.commandParser $ progDesc "Call rib"
      ]
  where
    newCommand =
      New <$> argument str (metavar "TITLE" <> help "Title of the new Zettel")

run :: Path Rel Dir -> Path Rel Dir -> Action () -> IO ()
run srcDir dstDir act = runWith srcDir dstDir act =<< execParser opts
  where
    opts =
      info
        (commandParser <**> helper)
        (fullDesc <> progDesc "Zettelkasten based on Rib")

runWith :: Path Rel Dir -> Path Rel Dir -> Action () -> Command -> IO ()
runWith srcDir dstDir act = \case
  New tit -> do
    s <- newZettelFile srcDir tit
    putStrLn s
  Rib c -> Rib.App.runWith srcDir dstDir act c

-- | Generate the Zettelkasten site
generateSite ::
  (Z.Route Z.ZettelStore Z.ZettelGraph () -> (Z.ZettelStore, Z.ZettelGraph) -> Action ()) ->
  [Path Rel File] ->
  Action ()
generateSite writeHtmlRoute' zettelsPat = do
  zettelStore <- Z.mkZettelStore =<< Rib.forEvery zettelsPat pure
  zettelGraph <- Z.mkZettelGraph zettelStore
  let writeHtmlRoute r = writeHtmlRoute' r (zettelStore, zettelGraph)
  (writeHtmlRoute . Z.Route_Zettel) `mapM_` Map.keys zettelStore
  writeHtmlRoute Z.Route_Index

-- | Create a new zettel file and return its slug
-- TODO: refactor this
newZettelFile :: Path Rel Dir -> Text -> IO String
newZettelFile inputDir ztitle = do
  zId <- zettelNextIdForToday
  baseName <- parseRelFile $ toString $ zId <> ".md"
  let srcPath = inputDir </> notesDir </> baseName
  doesFileExist srcPath >>= \case
    True ->
      fail $ "File already exists: " <> show srcPath
    False -> do
      writeFile (toFilePath srcPath) $ "---\ntitle: " <> toString ztitle <> "\n---\n\n"
      pure $ toFilePath srcPath
  where
    zettelNextIdForToday :: IO Text
    zettelNextIdForToday = do
      zIdPartial <- dayIndex . toText . formatTime defaultTimeLocale "%y%W%a" <$> getCurrentTime
      noteFiles <- listDirectory $ toFilePath $ inputDir </> notesDir
      let nums :: [Int] = sort $ catMaybes $ fmap readMaybe $ catMaybes $ catMaybes $ fmap (fmap listToMaybe . FP.match (toString zIdPartial <> "*.md")) $ noteFiles
      case fmap last (nonEmpty nums) of
        Just lastNum ->
          pure $ zIdPartial <> toText @String (printf "%02d" $ lastNum + 1)
        Nothing ->
          pure $ zIdPartial <> "01"
      where
        dayIndex =
          T.replace "Mon" "1"
            . T.replace "Tue" "2"
            . T.replace "Wed" "3"
            . T.replace "Thu" "4"
            . T.replace "Fri" "5"
            . T.replace "Sat" "6"
            . T.replace "Sun" "7"
    notesDir :: Path Rel Dir
    notesDir = [reldir|notes|]
