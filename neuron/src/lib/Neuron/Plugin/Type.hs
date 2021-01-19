{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Plugin.Type where

import qualified Commonmark as CM
import Control.Monad.Writer
import Data.Default (Default (..))
import qualified Data.YAML as Y
import Neuron.Frontend.Route (NeuronWebT)
import Neuron.Markdown
import Neuron.Zettelkasten.Connection (ContextualConnection)
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.ID (ZettelID)
import Neuron.Zettelkasten.Resolver (ZIDRef)
import Neuron.Zettelkasten.Zettel
import Reflex.Dom.Core (DomBuilder, PostBuild)
import Reflex.Dom.Pandoc (PandocBuilder)
import Reflex.Dom.Widget (blank)
import Relude
import qualified System.Directory.Contents.Types as DC
import Text.Pandoc.Definition (Pandoc)

data Plugin routeData = Plugin
  { -- | Markdown, custom parser
    _plugin_markdownSpec :: forall m il bl. NeuronSyntaxSpec m il bl => CM.SyntaxSpec m il bl,
    -- | Apply any filter on the source tree before beginning any processing
    _plugin_filterSources :: DC.DirTree FilePath -> IO (Maybe (DC.DirTree FilePath)),
    -- | Called after zettel files read into memory
    _plugin_afterZettelRead :: forall m. MonadState (Map ZettelID ZIDRef) m => DC.DirTree FilePath -> m (),
    -- | Called after zettel files are fully parsed
    _plugin_afterZettelParse :: Maybe (Y.Node Y.Pos) -> ZettelC -> ZettelC,
    -- | Called before building the graph. Allows the plugin to create new connections on demand.
    _plugin_graphConnections ::
      forall m.
      ( -- Running queries requires the zettels list.
        MonadReader [Zettel] m,
        -- Track missing zettel links in writer
        MonadWriter [MissingZettel] m
      ) =>
      Zettel ->
      m [(ContextualConnection, Zettel)],
    -- | Pre-compute all data required to render this plugin's view. Only at
    -- this stage, is the zettel graph made available.
    _plugin_routeData :: ZettelGraph -> ZettelC -> routeData,
    -- | Plugin-specific HTML rendering to do on the zettel pages.
    _plugin_renderPanel :: forall t m. (DomBuilder t m, PostBuild t m) => (Pandoc -> NeuronWebT t m ()) -> routeData -> NeuronWebT t m (),
    -- | Hooks for rendering custom DOM elements; here, url links.
    _plugin_renderHandleLink :: forall t m. (PandocBuilder t m, PostBuild t m) => routeData -> Text -> Maybe (NeuronWebT t m ()),
    -- | Strip data you don't want in JSON dumps
    _plugin_preJsonStrip :: Zettel -> Zettel
  }

instance Default a => Default (Plugin a) where
  def =
    Plugin
      { _plugin_markdownSpec = mempty,
        _plugin_filterSources = pure . Just,
        _plugin_afterZettelRead = void . pure,
        _plugin_afterZettelParse = const id,
        _plugin_graphConnections = const $ pure mempty,
        _plugin_routeData = def,
        _plugin_renderPanel = \_ _ -> blank,
        _plugin_renderHandleLink = \_ _ -> Nothing,
        _plugin_preJsonStrip = id
      }