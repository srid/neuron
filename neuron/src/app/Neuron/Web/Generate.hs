{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Main module for using neuron as a library, instead of as a CLI tool.
module Neuron.Web.Generate
  ( generateSite,
    loadZettelkasten,
  )
where

import Data.FileEmbed (embedStringFile)
import qualified Data.Map.Strict as Map
import Data.Tagged (untag)
import qualified Data.Text as T
import Data.Traversable
import Development.Shake (Action, need)
import Neuron.Config.Alias (Alias (..), getAliases)
import Neuron.Config.Type (Config (minVersion), getZettelFormats)
import Neuron.Reader (readerForZettelFormat)
import Neuron.Reader.Type (ZettelFormat, zettelFormatToExtension)
import Neuron.Version (neuronVersion, olderThan)
import Neuron.Web.Generate.Route ()
import qualified Neuron.Web.Route as Z
import qualified Neuron.Zettelkasten.Graph.Build as G
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.ID (ZettelID, getZettelID)
import Neuron.Zettelkasten.Query.Error (showQueryError)
import Neuron.Zettelkasten.Zettel
import Options.Applicative
import Relude
import qualified Rib
import Rib.Route
import System.FilePath

searchScript :: Text
searchScript = $(embedStringFile "./src-js/search.js")

-- | Generate the Zettelkasten site
generateSite ::
  Config ->
  (forall a. Z.Route a -> (ZettelGraph, a) -> Action ()) ->
  Action ZettelGraph
generateSite config writeHtmlRoute' = do
  when (olderThan $ minVersion config) $ do
    fail $ toString $
      "Require neuron mininum version "
        <> minVersion config
        <> ", but your neuron version is "
        <> neuronVersion
  (zettelGraph, zettelContents, errors) <- loadZettelkasten config
  let writeHtmlRoute :: forall a. a -> Z.Route a -> Action ()
      writeHtmlRoute v r = writeHtmlRoute' r (zettelGraph, v)
  -- Generate HTML for every zettel
  forM_ zettelContents $ \val@(sansContent -> z) ->
    writeHtmlRoute val $ Z.Route_Zettel (zettelID z)
  -- Generate the z-index
  writeHtmlRoute errors Z.Route_ZIndex
  -- Generate search page
  writeHtmlRoute searchScript $ Z.Route_Search Nothing
  -- Write alias redirects, unless a zettel with that name exists.
  aliases <- getAliases config zettelGraph
  forM_ aliases $ \Alias {..} ->
    writeHtmlRoute targetZettel (Z.Route_Redirect aliasZettel)
  -- Report all errors
  forM_ (Map.toList errors) $ \(zid, err) -> do
    reportError (Z.Route_Zettel zid) $
      case err of
        ZettelError_ParseError (untag -> parseErr) ->
          parseErr :| []
        ZettelError_QueryErrors queryErrs ->
          showQueryError <$> queryErrs
        ZettelError_AmbiguousFiles filePaths ->
          ("Multiple zettels have the same ID: " <> T.intercalate ", " (fmap toText $ toList filePaths))
            :| []
  pure zettelGraph

-- | Report an error in the terminal
reportError :: (MonadIO m, IsRoute r) => r a -> NonEmpty Text -> m ()
reportError route errors = do
  path <- liftIO $ routeFile route
  putTextLn $ "E " <> toText path
  forM_ errors $ \err ->
    putText $ "  - " <> indentAllButFirstLine 4 err
  where
    indentAllButFirstLine :: Int -> Text -> Text
    indentAllButFirstLine n = unlines . go . lines
      where
        go [] = []
        go [x] = [x]
        go (x : xs) =
          x : fmap (toText . (take n (repeat ' ') <>) . toString) xs

loadZettelkasten ::
  Config ->
  Action
    ( ZettelGraph,
      [ZettelC],
      Map ZettelID ZettelError
    )
loadZettelkasten config = do
  formats <- getZettelFormats config
  zettelFiles <- forM formats $ \fmt -> do
    let pat = toString $ "*" <> zettelFormatToExtension fmt
    files <- Rib.forEvery [pat] pure
    pure (fmt, files)
  loadZettelkastenFrom zettelFiles

-- | Load the Zettelkasten from disk, using the given list of zettel files
loadZettelkastenFrom ::
  NonEmpty (ZettelFormat, [FilePath]) ->
  Action
    ( ZettelGraph,
      [ZettelC],
      Map ZettelID ZettelError
    )
loadZettelkastenFrom fs = do
  notesDir <- Rib.ribInputDir
  -- Use State monad to "gather" duplicate zettel files using same IDs, in the
  -- `Right` of the Either state value; with the `Left` collecting the actual
  -- zettel files to load into the graph.
  zidMap :: Map ZettelID (Either (ZettelFormat, (FilePath, Text)) (NonEmpty FilePath)) <-
    fmap snd $ flip runStateT Map.empty $ do
      forM_ fs $ \(format, files) -> do
        forM_ files $ \relPath -> do
          case getZettelID relPath of
            Nothing ->
              pure ()
            Just zid -> do
              fmap (Map.lookup zid) get >>= \case
                Just (Left (_f, (oldPath, _s))) -> do
                  -- The zettel ID is already used by `oldPath`. Mark it as a dup.
                  modify $ Map.insert zid (Right $ relPath :| [oldPath])
                Just (Right (toList -> ambiguities)) -> do
                  -- Third or later duplicate file with the same Zettel ID
                  modify $ Map.insert zid (Right $ relPath :| ambiguities)
                Nothing -> do
                  let absPath = notesDir </> relPath
                  lift $ need [absPath]
                  s <- decodeUtf8With lenientDecode <$> readFileBS absPath
                  modify $ Map.insert zid (Left (format, (relPath, s)))
  let dups = fmap ZettelError_AmbiguousFiles $ Map.mapMaybe rightToMaybe zidMap
      files =
        fmap (first (id &&& readerForZettelFormat))
          $ Map.toList
          $ Map.fromListWith (<>)
          $ flip fmap (Map.toList $ Map.mapMaybe leftToMaybe zidMap)
          $ \(zid, (fmt, (path, s))) ->
            (fmt, [(zid, path, s)])
      (g, zs, gerrs) = G.buildZettelkasten files
      errs = Map.unions [dups, gerrs]
  pure (g, zs, errs)
