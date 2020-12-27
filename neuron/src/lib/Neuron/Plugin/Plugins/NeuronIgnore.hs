{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Plugin.Plugins.NeuronIgnore (plugin) where

import Control.Monad.Writer (runWriter)
import Control.Monad.Writer.Strict (MonadWriter (tell))
import Data.Default (Default (def))
import qualified Data.Text as T
import Neuron.Plugin.Type (Plugin (..))
import Reflex.Dom.Core (fforMaybe)
import Relude hiding (trace, traceShow, traceShowId)
import qualified System.Directory.Contents as DC
import System.FilePath.Posix (takeExtension)
import System.FilePattern (FilePattern, (?==))

plugin :: Plugin ()
plugin =
  def
    { _plugin_filterSources = applyNeuronIgnore
    }

-- | Ignore files based on the top-level .neuronignore file. If the file does
-- not exist, apply the default patterns.
applyNeuronIgnore :: DC.DirTree FilePath -> IO (Maybe (DC.DirTree FilePath))
applyNeuronIgnore t = do
  -- Note that filterDirTree invokes the function only files, not directory paths
  -- FIXME: `filterADirTree` unfortunately won't filter directories; so
  -- even if a top-level directory is configured to be ignored, this
  -- filter will traverse that entire directory tree to apply the glob
  -- pattern filter.
  -- TODO: neuron should detect changes to this file, and reload.
  ignorePats :: [FilePattern] <- case DC.walkDirTree "./.neuronignore" t of
    Just (DC.DirTree_File _ fp) -> do
      ls <- T.lines <$> readFileText fp
      pure $
        fforMaybe ls $ \(T.strip -> s) -> do
          guard $ not $ T.null s
          -- Ignore line comments
          guard $ not $ "#" `T.isPrefixOf` s
          pure $ toString s
    _ ->
      pure defaultIgnorePats
  putTextLn $ "Ignore patterns: " <> show ignorePats
  let (mTreeFiltered, _nExcluded) = runWriter @[FilePath] $ DC.filterADirTree (includeDirEntry ignorePats) t
  -- Not printing, because this includesd all non-Markdown files, including static files. Hence, not really accurate.
  -- liftIO $ putStrLn $ "Excluded " <> show nExcluded <> " filepaths"
  pure $ DC.pruneDirTree =<< mTreeFiltered
  where
    defaultIgnorePats =
      [ -- Ignore top-level dotfiles and dotdirs
        "./.*/**"
        -- Ignore everything under sub directories
        -- "./*/*/**"
      ]
    includeDirEntry pats name = do
      let included =
            Just True
              == ( do
                     guard $ not $ any (?== name) pats
                     guard $ takeExtension name == ".md"
                     pure True
                 )
      unless included $ tell [name]
      pure included
