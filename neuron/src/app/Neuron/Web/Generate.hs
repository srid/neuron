{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Main module for using neuron as a library, instead of as a CLI tool.
module Neuron.Web.Generate
  ( generateSite,
    loadZettelkasten,
    loadZettelsIgnoringErrors,
  )
where

import qualified Data.Map.Strict as Map
import Data.Traversable
import Development.Shake
import Neuron.Config (Config (..))
import Neuron.Config.Alias (Alias (..), getAliases)
import Neuron.Version (neuronVersion, olderThan)
import Neuron.Web.Generate.Route ()
import qualified Neuron.Web.Route as Z
import qualified Neuron.Zettelkasten.Graph as G
import qualified Neuron.Zettelkasten.Graph.Build as G
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.ID (ZettelID)
import Neuron.Zettelkasten.Query.Error (showQueryError)
import Neuron.Zettelkasten.Zettel
import Neuron.Zettelkasten.Zettel.Parser
import Options.Applicative
import Relude
import qualified Rib
import Rib.Route
import System.FilePath

-- | Generate the Zettelkasten site
generateSite ::
  Config ->
  (forall a. Z.Route a -> (ZettelGraph, a) -> Action ()) ->
  Action ZettelGraph
generateSite config writeHtmlRoute' = do
  when (olderThan $ minVersion config)
    $ fail
    $ toString
    $ "Require neuron mininum version " <> minVersion config <> ", but your neuron version is " <> neuronVersion
  (zettelGraph, zettelContents, errors) <- loadZettelkasten
  let writeHtmlRoute :: forall a. a -> Z.Route a -> Action ()
      writeHtmlRoute v r = writeHtmlRoute' r (zettelGraph, v)
  -- Generate HTML for every zettel
  forM_ zettelContents $ \val@(sansContent -> z) -> do
    let r = Z.Route_Zettel $ zettelID z
    writeHtmlRoute val r
  -- Generate the z-index
  writeHtmlRoute errors Z.Route_ZIndex
  -- Generate search page
  writeHtmlRoute () Z.Route_Search
  -- Write alias redirects, unless a zettel with that name exists.
  aliases <- getAliases config zettelGraph
  forM_ aliases $ \Alias {..} ->
    writeHtmlRoute targetZettel (Z.Route_Redirect aliasZettel)
  forM_ (Map.toList errors) $ \(zid, eerr) -> do
    case eerr of
      Left serr ->
        reportError (Z.Route_Zettel zid) (Just "SKIPPED") [show serr]
      Right qerrs ->
        reportError (Z.Route_Zettel zid) Nothing $ showQueryError <$> toList qerrs
  pure zettelGraph

-- | Report an error in the terminal
reportError :: (MonadIO m, IsRoute r) => r a -> Maybe Text -> [Text] -> m ()
reportError route mErrorKind errors = do
  putTextLn $ "E " <> fromMaybe "Unknown route" (fmap toText $ routeFile route) <> maybe "" (\x -> " (" <> x <> ")") mErrorKind
  forM_ errors $ \err ->
    putText $ "  - " <> indentAllButFirstLine 4 err

indentAllButFirstLine :: Int -> Text -> Text
indentAllButFirstLine n = unlines . go . lines
  where
    go [] = []
    go [x] = [x]
    go (x : xs) =
      x : fmap (toText . (take n (repeat ' ') <>) . toString) xs

loadZettelsIgnoringErrors :: Action [Zettel]
loadZettelsIgnoringErrors = do
  (g, _, _) <- loadZettelkasten
  pure $ G.getZettels g

loadZettelkasten ::
  Action
    ( ZettelGraph,
      [ZettelC],
      Map ZettelID ZettelError
    )
loadZettelkasten =
  loadZettelkastenFrom =<< Rib.forEvery ["*.md"] pure

-- | Load the Zettelkasten from disk, using the given list of zettel files
loadZettelkastenFrom ::
  [FilePath] ->
  Action
    ( ZettelGraph,
      [ZettelC],
      Map ZettelID ZettelError
    )
loadZettelkastenFrom files = do
  notesDir <- Rib.ribInputDir
  filesWithContent <- forM files $ \((notesDir </>) -> path) -> do
    need [path]
    s <- decodeUtf8With lenientDecode <$> readFileBS path
    pure (path, s)
  let zs = parseZettels filesWithContent
      skippedErrors = Map.fromList $ flip mapMaybe zs $ \case
        Left (Zettel {..}) -> Just (zettelID, zettelError)
        Right _ -> Nothing
      (g, Map.mapMaybe nonEmpty -> queryErrors) = G.mkZettelGraph $ fmap sansContent zs
  pure (g, zs, fmap Left skippedErrors `Map.union` fmap Right queryErrors)
