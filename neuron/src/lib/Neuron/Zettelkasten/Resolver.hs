{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Transform a directory of Markdown files to Zettelkasten-strict zettel texts.
--
-- This module is responsible only for "resolving" the filename IDs. It will
-- return file contents as is (i.e., as text), without doing any parsing itself.
-- It can also be used by plugins.
module Neuron.Zettelkasten.Resolver where

import Data.Dependent.Map (DMap)
import qualified Data.Map.Strict as Map
import Neuron.Plugin.PluginData (PluginData)
import Neuron.Zettelkasten.ID (ZettelID, getZettelID)
import Relude
import qualified System.Directory.Contents.Types as DC

-- | What does a Zettel ID refer to?
data ZIDRef
  = -- | The ZID maps to a file on disk with the given contents
    ZIDRef_Available FilePath Text (DMap PluginData Identity)
  | -- | The ZID maps to more than one file, hence ambiguous.
    ZIDRef_Ambiguous (NonEmpty FilePath)
  deriving (Eq, Show)

resolveZidRefsFromDirTree :: Monad m => (FilePath -> m Text) -> DC.DirTree FilePath -> StateT (Map ZettelID ZIDRef) m ()
resolveZidRefsFromDirTree readFileF = \case
  DC.DirTree_File relPath _ -> do
    whenJust (getZettelID relPath) $ \zid -> do
      addZettel relPath zid mempty $
        lift $ readFileF relPath
  DC.DirTree_Dir _absPath contents -> do
    forM_ (Map.toList contents) $ \(_, ct) ->
      resolveZidRefsFromDirTree readFileF ct
  _ ->
    -- We ignore symlinks, and paths configured to be excluded.
    pure ()

addZettel :: MonadState (Map ZettelID ZIDRef) m => FilePath -> ZettelID -> DMap PluginData Identity -> m Text -> m ()
addZettel zpath zid pluginData ms = do
  gets (Map.lookup zid) >>= \case
    Just (ZIDRef_Available oldPath _s _m) -> do
      -- The zettel ID is already used by `oldPath`. Mark it as a dup.
      modify $ Map.insert zid (ZIDRef_Ambiguous $ zpath :| [oldPath])
    Just (ZIDRef_Ambiguous (toList -> ambiguities)) -> do
      -- Third or later duplicate file with the same Zettel ID
      markAmbiguous zid $ zpath :| ambiguities
    Nothing -> do
      s <- ms
      modify $ Map.insert zid (ZIDRef_Available zpath s pluginData)

markAmbiguous :: (MonadState (Map ZettelID ZIDRef) m) => ZettelID -> NonEmpty FilePath -> m ()
markAmbiguous zid fs =
  modify $ Map.insert zid (ZIDRef_Ambiguous fs)