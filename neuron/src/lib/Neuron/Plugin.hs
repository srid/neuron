{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Plugin where

import Clay (Css, (?))
import qualified Clay as C
import qualified Commonmark as CM
import Control.Monad.Writer
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum (..))
import qualified Data.Graph.Labelled as Algo
import qualified Data.Map.Strict as Map
import Data.Some
import qualified Data.Text as T
import Neuron.Frontend.Route (NeuronWebT)
import Neuron.Frontend.Route.Data.Types
import Neuron.Markdown (NeuronSyntaxSpec)
import qualified Neuron.Plugin.Plugins.DirTree as DirTree
import qualified Neuron.Plugin.Plugins.Links as Links
import qualified Neuron.Plugin.Plugins.NeuronIgnore as NeuronIgnore
import qualified Neuron.Plugin.Plugins.Tags as Tags
import qualified Neuron.Plugin.Plugins.UpTree as UpTree
import Neuron.Plugin.Type (Plugin (..))
import Neuron.Zettelkasten.Connection (ContextualConnection)
import Neuron.Zettelkasten.Format
import Neuron.Zettelkasten.Format.Reader.Type
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.ID (ZettelID)
import Neuron.Zettelkasten.Resolver (ZIDRef)
import Neuron.Zettelkasten.Zettel
  ( MissingZettel,
    PluginZettelData (..),
    Zettel,
    ZettelC,
    ZettelT (zettelPluginData),
  )
import Neuron.Zettelkasten.Zettel.Parser (parseZettels)
import Reflex.Dom.Core (DomBuilder, PostBuild)
import Reflex.Dom.Pandoc (PandocBuilder)
import Reflex.Dom.Widget (blank)
import Relude
import qualified System.Directory.Contents.Types as DC
import Text.Pandoc.Definition (Inline, Pandoc)

type PluginRegistry = Map (Some PluginZettelData) (Some Plugin)

pluginRegistryShow :: PluginRegistry -> Text
pluginRegistryShow r =
  T.intercalate ", " $
    Map.keys r <&> \case
      Some DirTree -> "dirtree"
      Some Links -> "links"
      Some Tags -> "tags"
      Some NeuronIgnore -> "neuronignore"
      Some UpTree -> "uptree"

lookupPlugins :: [Text] -> PluginRegistry
lookupPlugins = Map.fromList . mapMaybe lookupPlugin
  where
    lookupPlugin :: Text -> Maybe (Some PluginZettelData, Some Plugin)
    lookupPlugin = \case
      "dirtree" -> Just (Some DirTree, Some DirTree.plugin)
      "links" -> Just (Some Links, Some Links.plugin)
      "tags" -> Just (Some Tags, Some Tags.plugin)
      "neuronignore" -> Just (Some NeuronIgnore, Some NeuronIgnore.plugin)
      "uptree" -> Just (Some UpTree, Some UpTree.plugin)
      _ -> Nothing

markdownSpec :: NeuronSyntaxSpec m il bl => PluginRegistry -> CM.SyntaxSpec m il bl
markdownSpec plugins =
  mconcat $ Map.elems plugins <&> \sp -> withSome sp _plugin_markdownSpec

filterSources :: PluginRegistry -> DC.DirTree FilePath -> IO (Maybe (DC.DirTree FilePath))
filterSources plugins t = do
  let applyF = Map.elems plugins <&> \sp -> withSome sp _plugin_filterSources
      combiner :: (Traversable m, Monad m) => IO (m a) -> (a -> IO (m b)) -> IO (m b)
      combiner = \ima f -> (fmap join . traverse f) =<< ima
  foldl' combiner (pure $ Just t) applyF

afterZettelParse :: PluginRegistry -> (ZettelFormat -> ZettelParser) -> [(ZettelID, (FilePath, ZettelFormat, (Text, DMap PluginZettelData Identity)))] -> [ZettelC]
afterZettelParse plugins getParser fs = do
  let h =
        Map.elems plugins <&> \sp ->
          withSome sp $ \p (m, x) -> (m,) $ _plugin_afterZettelParse p m x
  parseZettels getParser fs <&> \x ->
    snd $ foldl' (\x1 f -> f x1) x h

afterZettelRead :: MonadState (Map ZettelID ZIDRef) m => PluginRegistry -> DC.DirTree FilePath -> m ()
afterZettelRead plugins fileTree = do
  forM_ (plugins <&> \sp -> withSome sp _plugin_afterZettelRead) $ \f -> f fileTree

graphConnections ::
  forall m.
  ( -- Running queries requires the zettels list.
    MonadReader [Zettel] m,
    -- Track missing zettel links in writer
    MonadWriter [MissingZettel] m
  ) =>
  PluginRegistry ->
  Zettel ->
  m [(ContextualConnection, Zettel)]
graphConnections plugins z = do
  fmap concat $ forM (plugins <&> \sp -> withSome sp _plugin_graphConnections) $ \f -> f z

pluginStyles ::
  PluginRegistry ->
  Css
pluginStyles plugins =
  C.body ? do
    mconcat $ Map.elems plugins <&> \sp -> withSome sp _plugin_css

-- TODO: Use _plugin_* functions directly!
routePluginData :: ZettelGraph -> ZettelC -> DSum PluginZettelData Identity -> DSum PluginZettelRouteData Identity
routePluginData g z = \case
  DirTree :=> Identity dirTree ->
    PluginZettelRouteData_DirTree :=> Identity (DirTree.routePluginData g dirTree)
  Links :=> Identity x ->
    PluginZettelRouteData_Links :=> Identity (Links.routePluginData g z x)
  Tags :=> Identity x ->
    PluginZettelRouteData_Tags :=> Identity (Tags.routePluginData g z x)
  NeuronIgnore :=> Identity () ->
    PluginZettelRouteData_NeuronIgnore :=> Identity ()
  UpTree :=> Identity () ->
    PluginZettelRouteData_UpTree :=> Identity (UpTree.routePluginData g z)

renderPluginPanel ::
  (DomBuilder t m, PostBuild t m) =>
  (Pandoc -> NeuronWebT t m ()) ->
  Zettel ->
  DSum PluginZettelRouteData Identity ->
  NeuronWebT t m ()
renderPluginPanel elNeuronPandoc z = \case
  PluginZettelRouteData_DirTree :=> Identity t ->
    DirTree.renderPanel t
  PluginZettelRouteData_Links :=> Identity x ->
    Links.renderPanel elNeuronPandoc x
  PluginZettelRouteData_Tags :=> Identity x ->
    Tags.renderPanel elNeuronPandoc z x
  _ ->
    blank

-- TODO: Consolidate this, and renderPluginPanel, into one renderer function,
-- possibly using an ADT do differentiate between different "locations" in UI
-- tree.
renderPluginTop ::
  (DomBuilder t m, PostBuild t m) =>
  DSum PluginZettelRouteData Identity ->
  NeuronWebT t m ()
renderPluginTop = \case
  PluginZettelRouteData_UpTree :=> Identity t ->
    UpTree.render t
  _ ->
    blank

renderHandleLink ::
  (PandocBuilder t m, PostBuild t m) =>
  DMap PluginZettelRouteData Identity ->
  Text ->
  Maybe [Inline] ->
  Maybe (NeuronWebT t m ())
renderHandleLink pluginData url mInline =
  asum $ DMap.toList pluginData <&> \x -> renderHandleLink' x url mInline

renderHandleLink' ::
  (PandocBuilder t m, PostBuild t m) =>
  DSum PluginZettelRouteData Identity ->
  Text ->
  Maybe [Inline] ->
  Maybe (NeuronWebT t m ())
renderHandleLink' = \case
  PluginZettelRouteData_Links :=> Identity x ->
    Links.renderHandleLink x
  PluginZettelRouteData_Tags :=> Identity x ->
    Tags.renderHandleLink x
  _ ->
    const $ const Nothing

preJsonStrip :: Zettel -> Zettel
preJsonStrip z =
  let pluginData' = flip fmap (DMap.toList $ zettelPluginData z) $ \case
        x@(NeuronIgnore :=> Identity ()) ->
          x
        x@(DirTree :=> Identity _) ->
          x
        x@(Tags :=> Identity _) ->
          x
        x@(UpTree :=> Identity _) ->
          x
        Links :=> Identity x ->
          Links :=> Identity (Links.preJsonStrip x)
   in z {zettelPluginData = DMap.fromList pluginData'}

-- | Compress the graph to save space, by eliminating the unnecessary
-- surrounding context Pandoc blocks.
stripSurroundingContext :: ZettelGraph -> ZettelGraph
stripSurroundingContext =
  Algo.emap (fmap (second $ const mempty)) . Algo.vmap preJsonStrip
