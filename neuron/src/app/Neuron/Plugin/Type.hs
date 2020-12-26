{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Plugin.Type where

import Development.Shake (Action)
import Neuron.Zettelkasten.ID (ZettelID)
import Neuron.Zettelkasten.Resolver (ZIDRef)
import Neuron.Zettelkasten.Zettel (ZettelC)
import Relude
import qualified System.Directory.Contents as DC

data Plugin = Plugin
  { _plugin_name :: Text,
    -- | Called after zettel files read in memory
    _plugin_afterZettelRead :: DC.DirTree FilePath -> StateT (Map ZettelID ZIDRef) Action (),
    -- | Called after zettel files are parsed
    _plugin_afterZettelParse :: ZettelC -> ZettelC
  }
