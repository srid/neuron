{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Plugin.Type where

import Neuron.Web.Route (NeuronWebT)
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.ID (ZettelID)
import Neuron.Zettelkasten.Resolver (ZIDRef)
import Neuron.Zettelkasten.Zettel (ZettelC)
import Reflex.Dom.Core (DomBuilder)
import Relude
import qualified System.Directory.Contents.Types as DC

data Plugin pluginData = Plugin
  { _plugin_name :: Text,
    -- | Called after zettel files read in memory
    _plugin_afterZettelRead :: forall m. MonadState (Map ZettelID ZIDRef) m => DC.DirTree FilePath -> m (),
    -- | Called after zettel files are parsed
    _plugin_afterZettelParse :: ZettelC -> ZettelC,
    -- | FIXME: can't be Maybe Tag, that's DF specific
    _plugin_renderPanel :: forall t m. DomBuilder t m => ZettelGraph -> pluginData -> NeuronWebT t m ()
  }
