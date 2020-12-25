{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
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

import Control.Monad.Writer.Strict (runWriter, tell)
import qualified Data.List
import qualified Data.Map.Strict as Map
import Data.TagTree (Tag (unTag))
import Data.Tagged (untag)
import qualified Data.Text as T
import Debug.Trace (traceShowId)
import Development.Shake (Action, need)
import Neuron.Config.Type (Config)
import qualified Neuron.Plugin.DirectoryFolgezettel as DF
import Neuron.Version (neuronVersion)
import qualified Neuron.Web.Cache as Cache
import Neuron.Web.Cache.Type (NeuronCache, _neuronCache_graph)
import qualified Neuron.Web.Cache.Type as Cache
import Neuron.Web.Generate.Route ()
import qualified Neuron.Web.Route as Z
import qualified Neuron.Zettelkasten.Graph.Build as G
import Neuron.Zettelkasten.Graph.Type (ZettelGraph, stripSurroundingContext)
import Neuron.Zettelkasten.ID (ZettelID (ZettelID), getZettelID, unZettelID)
import Neuron.Zettelkasten.Query.Error (showQueryResultError)
import Neuron.Zettelkasten.Zettel
  ( ZettelC,
    ZettelError (..),
    ZettelT (zettelSlug),
    sansContent,
  )
import Neuron.Zettelkasten.Zettel.Parser (extractQueriesWithContext, parseZettels)
import Relude hiding (traceShowId)
import Rib.Shake (ribInputDir)
import System.Directory (withCurrentDirectory)
import qualified System.Directory.Contents as DC
import System.FilePath (takeDirectory, takeExtension, takeFileName, (</>))

-- | Generate the Zettelkasten site
generateSite ::
  Config ->
  (forall a. NeuronCache -> Z.Route a -> a -> Action ()) ->
  Action ZettelGraph
generateSite config writeHtmlRoute' = do
  (cache@Cache.NeuronCache {..}, zettelContents) <- loadZettelkasten config
  let writeHtmlRoute :: forall a. a -> Z.Route a -> Action ()
      writeHtmlRoute v r = writeHtmlRoute' cache r v
  -- Generate HTML for every zettel
  forM_ zettelContents $ \val@(zettelSlug . sansContent -> slug) ->
    writeHtmlRoute val $ Z.Route_Zettel slug
  -- Generate search page
  writeHtmlRoute () $ Z.Route_Impulse Nothing
  -- Report all errors
  -- TODO: Report only new errors in this run, to avoid spamming the terminal.
  forM_ (Map.toList _neuronCache_errors) $ \(zid, err) -> do
    reportError zid $
      case err of
        ZettelError_ParseError (untag . snd -> parseErr) ->
          parseErr :| []
        ZettelError_QueryResultErrors queryErrs ->
          showQueryResultError <$> snd queryErrs
        ZettelError_AmbiguousID filePaths ->
          ("Multiple zettels have the same ID: " <> T.intercalate ", " (toText <$> toList filePaths))
            :| []
        ZettelError_AmbiguousSlug slug ->
          "Slug '" <> slug <> "' is already used by another zettel" :| []
  pure _neuronCache_graph
  where
    -- Report an error in the terminal
    reportError :: MonadIO m => ZettelID -> NonEmpty Text -> m ()
    reportError zid errors = do
      putTextLn $ "E " <> unZettelID zid
      forM_ errors $ \err ->
        putText $ "  - " <> indentAllButFirstLine 4 err
      where
        indentAllButFirstLine :: Int -> Text -> Text
        indentAllButFirstLine n = unlines . go . lines
          where
            go [] = []
            go [x] = [x]
            go (x : xs) =
              x : fmap (toText . (replicate n ' ' <>) . toString) xs

loadZettelkasten :: Config -> Action (NeuronCache, [ZettelC])
loadZettelkasten config = do
  ((g, zs), errs) <-
    loadZettelkastenFromFiles . makeDirectoryFolgezettels =<< locateZettelFiles
  let cache = Cache.NeuronCache g errs config neuronVersion
      cacheSmall = cache {_neuronCache_graph = stripSurroundingContext g}
  Cache.updateCache cacheSmall
  pure (cache, zs)
  where
    -- TODO:
    -- If making recurseDir the default,
    -- - [ ] Allow blacklist (eg: not "README.md"; default being ".*"; always ignore .neuron)
    locateZettelFiles = do
      -- Run with notes dir as PWD, so that DirTree uses relative paths throughout.
      inNotesDir $
        DC.buildDirTree "." >>= \case
          Just t@(DC.DirTree_Dir _notesDir' _toplevel) -> do
            let mt' = DC.pruneDirTree =<< DC.filterDirTree includeDirEntry t
            maybe (fail "No markdown files?") pure mt'
          _ -> fail "Directory error?"
    includeDirEntry name =
      Just True
        == ( do
               guard $ not $ isDotfile name
               guard $ takeExtension name == ".md"
               pure True
           )
    isDotfile name =
      "." `Data.List.isPrefixOf` name
        && not ("./" `Data.List.isPrefixOf` name)

-- TODO: Move to Plugin/Neuron/*
makeDirectoryFolgezettels :: DC.DirTree FilePath -> DC.DirTree FilePath
makeDirectoryFolgezettels =
  -- TODO
  id

inNotesDir :: IO b -> Action b
inNotesDir a = do
  d <- ribInputDir
  liftIO $ withCurrentDirectory d a

-- | Load the Zettelkasten from disk, using the given list of zettel files
loadZettelkastenFromFiles ::
  DC.DirTree FilePath ->
  Action
    ( ( ZettelGraph,
        [ZettelC]
      ),
      Map ZettelID ZettelError
    )
loadZettelkastenFromFiles fileTree = do
  let total = getSum @Int $ foldMap (const $ Sum 1) fileTree
  -- TODO: Would be nice to show a progressbar here
  liftIO $ putStrLn $ "Loading directory tree (" <> show total <> " files) ..."
  zidRefs <-
    fmap snd $
      flip runStateT Map.empty $ do
        resolveZidRefsFromDirTree fileTree
        injectDirectoryFolgezettels fileTree
  pure $
    runWriter $ do
      filesWithContent <-
        flip Map.traverseMaybeWithKey zidRefs $ \zid -> \case
          ZIDRef_Ambiguous fps -> do
            tell $ one (zid, ZettelError_AmbiguousID fps)
            pure Nothing
          ZIDRef_Available fp s ->
            pure $ Just (fp, s)
      let zs = parseZettels extractQueriesWithContext $ Map.toList filesWithContent
      g <- G.buildZettelkasten zs
      pure (g, zs)

-- | What does a Zettel ID refer to?
data ZIDRef
  = -- | The ZID maps to a file on disk with the given contents
    ZIDRef_Available FilePath Text
  | -- | The ZID maps to more than one file, hence ambiguous.
    ZIDRef_Ambiguous (NonEmpty FilePath)
  deriving (Eq, Show)

resolveZidRefsFromDirTree :: DC.DirTree FilePath -> StateT (Map ZettelID ZIDRef) Action ()
resolveZidRefsFromDirTree = \case
  DC.DirTree_File relPath _ -> do
    whenJust (getZettelID relPath) $ \zid -> do
      addZettel relPath zid $
        lift $ do
          -- NOTE: This is the only place where Shake is being used (for
          -- posterity)
          absPath <- fmap (</> relPath) ribInputDir
          need [absPath]
          decodeUtf8With lenientDecode <$> readFileBS absPath
  DC.DirTree_Dir _absPath contents -> do
    forM_ (Map.toList contents) $ \(_, ct) ->
      resolveZidRefsFromDirTree ct
  _ ->
    -- We ignore symlinks, and paths configured to be excluded.
    pure ()

-- TODO: move to plugin module
injectDirectoryFolgezettels :: DC.DirTree FilePath -> StateT (Map ZettelID ZIDRef) Action ()
injectDirectoryFolgezettels = \case
  DC.DirTree_File _relPath _ -> do
    pure ()
  DC.DirTree_Dir absPath contents -> do
    let dirName = takeFileName absPath
    let dirZettelId = ZettelID $ toText $ if dirName == "." then "index" else dirName
    gets (Map.lookup dirZettelId) >>= \case
      Just ref -> do
        case ref of
          ZIDRef_Available p s -> do
            let s' = s <> directoryZettelContents absPath
            modify $ Map.update (const $ Just $ ZIDRef_Available p s') dirZettelId
          ZIDRef_Ambiguous {} ->
            -- TODO: What do do here?
            pure ()
      Nothing -> do
        addZettel ("<dirfolge:autogen:" <> absPath <> ">") dirZettelId $ do
          let header = "# " <> toText (takeFileName absPath) <> "/\n\n"
          pure $ header <> directoryZettelContents absPath
    forM_ (Map.toList contents) $ \(_, ct) ->
      injectDirectoryFolgezettels ct
  _ ->
    -- We ignore symlinks, and paths configured to be excluded.
    pure ()
  where
    directoryZettelContents absPath =
      let thisTag = case takeDirectory absPath of
            "." -> "index"
            x -> unTag $ DF.tagFromPath x
          inlineTags = "#dirfolge #" <> thisTag
          -- TODO: don't inject h1 title inline!
          -- should just do arbitrary type?
          md = "[[[z:zettels?tag=" <> unTag (DF.tagFromPath absPath) <> "]]]\n"
       in md <> "\n\n" <> inlineTags

addZettel :: MonadState (Map ZettelID ZIDRef) m => FilePath -> ZettelID -> m Text -> m ()
addZettel zpath zid ms = do
  gets (Map.lookup zid) >>= \case
    Just (ZIDRef_Available oldPath _s) -> do
      -- The zettel ID is already used by `oldPath`. Mark it as a dup.
      modify $ Map.insert zid (ZIDRef_Ambiguous $ zpath :| [oldPath])
    Just (ZIDRef_Ambiguous (toList -> ambiguities)) -> do
      -- Third or later duplicate file with the same Zettel ID
      modify $ Map.insert zid (ZIDRef_Ambiguous $ zpath :| ambiguities)
    Nothing -> do
      s <- ms
      modify $ Map.insert zid (ZIDRef_Available zpath s)

-- TODO: remove
_ignore :: forall a. Show a => a -> a
_ignore = traceShowId