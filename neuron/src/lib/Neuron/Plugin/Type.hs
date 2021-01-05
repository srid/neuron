{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Plugin.Type where

import Data.Default (Default (..))
import Neuron.Frontend.Route (NeuronWebT)
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.ID (ZettelID)
import Neuron.Zettelkasten.Resolver (ZIDRef)
import Neuron.Zettelkasten.Zettel (ZettelC)
import Reflex.Dom.Core (DomBuilder, PostBuild)
import Reflex.Dom.Widget (blank)
import Relude
import qualified System.Directory.Contents.Types as DC

data Plugin routeData = Plugin
  { -- | Apply any filter on the source tree before beginning any processing
    _plugin_filterSources :: DC.DirTree FilePath -> IO (Maybe (DC.DirTree FilePath)),
    -- | Called after zettel files read into memory
    _plugin_afterZettelRead :: forall m. MonadState (Map ZettelID ZIDRef) m => DC.DirTree FilePath -> m (),
    -- | Called after zettel files are fully parsed
    _plugin_afterZettelParse :: ZettelC -> ZettelC,
    -- | Pre-compute all data required to render this plugin's view. Only at
    -- this stage, is the zettel graph made available.
    _plugin_routeData :: ZettelGraph -> ZettelC -> routeData,
    -- | Plugin-specific HTML rendering to do on the zettel pages.
    _plugin_renderPanel :: forall t m. (DomBuilder t m, PostBuild t m) => routeData -> NeuronWebT t m ()
  }

instance Default a => Default (Plugin a) where
  def =
    Plugin
      { _plugin_filterSources = pure . Just,
        _plugin_afterZettelRead = void . pure,
        _plugin_afterZettelParse = id,
        _plugin_routeData = def,
        _plugin_renderPanel = const blank
      }