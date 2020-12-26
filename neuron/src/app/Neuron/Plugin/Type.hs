{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Plugin.Type where

import Neuron.Zettelkasten.ID (ZettelID)
import Neuron.Zettelkasten.Resolver (ZIDRef)
import Neuron.Zettelkasten.Zettel (ZettelC)
import Relude
import qualified System.Directory.Contents.Types as DC

-- TODO: Move to lib, after discarding shake Action dependency.
data Plugin = Plugin
  { _plugin_name :: Text,
    -- | Called after zettel files read in memory
    _plugin_afterZettelRead :: forall m. MonadState (Map ZettelID ZIDRef) m => DC.DirTree FilePath -> m (),
    -- | Called after zettel files are parsed
    _plugin_afterZettelParse :: ZettelC -> ZettelC
  }
