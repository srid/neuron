{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Plugin.Plugins.NeuronIgnore
  ( plugin,
    shouldIgnore,
    mandatoryIgnorePats,
  )
where

import Data.Default (Default (def))
import qualified Data.Text as T
import Neuron.Plugin.Type (Plugin (..))
import Reflex.Dom.Core (fforMaybe)
import Relude hiding (trace, traceShow, traceShowId)
import qualified System.Directory.Contents as DC
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
  -- FIXME(performance): `filterADirTree` unfortunately won't filter directories; so
  -- even if a top-level directory is configured to be ignored, this
  -- filter will traverse that entire directory tree to apply the glob
  -- pattern filter.
  ignorePats :: [FilePattern] <- fmap (mandatoryIgnorePats <>) $ case DC.walkDirTree "./.neuronignore" t of
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
  let mTreeFiltered = DC.filterDirTree (includeDirEntry ignorePats) t
  pure $ DC.pruneDirTree =<< mTreeFiltered
  where
    defaultIgnorePats =
      [ -- Ignore dotfiles and dotdirs
        "**/.*/**"
        -- Ignore everything under sub directories
        -- "*/*/**"
      ]
    includeDirEntry pats name =
      Just True
        == ( do
               guard $ not $ shouldIgnore pats name
               pure True
           )

shouldIgnore :: [FilePattern] -> FilePath -> Bool
shouldIgnore (fmap ("./" <>) -> pats) fp =
  any (?== fp) pats

mandatoryIgnorePats :: [FilePattern]
mandatoryIgnorePats =
  [ "**/.neuron/**",
    "**/.git/**"
  ]
