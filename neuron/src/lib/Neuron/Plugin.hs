{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Plugin where

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
import Neuron.Markdown (NeuronSyntaxSpec, parseMarkdown)
import qualified Neuron.Plugin.Plugins.DirTree as DirTree
import qualified Neuron.Plugin.Plugins.Links as Links
import qualified Neuron.Plugin.Plugins.NeuronIgnore as NeuronIgnore
import qualified Neuron.Plugin.Plugins.Tags as Tags
import Neuron.Plugin.Type (Plugin (..))
import Neuron.Zettelkasten.Connection (ContextualConnection)
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.ID (ZettelID)
import Neuron.Zettelkasten.Resolver (ZIDRef)
import Neuron.Zettelkasten.Zettel
import Neuron.Zettelkasten.Zettel.Parser (parseZettels)
import Reflex.Dom.Core (DomBuilder, PostBuild)
import Reflex.Dom.Pandoc (PandocBuilder)
import Reflex.Dom.Widget (blank)
import Relude
import qualified System.Directory.Contents.Types as DC
import Text.Pandoc.Definition (Pandoc)

type PluginRegistry = Map (Some PluginZettelData) (Some Plugin)

pluginRegistryShow :: PluginRegistry -> Text
pluginRegistryShow r =
  T.intercalate ", " $
    Map.keys r <&> \case
      Some PluginZettelData_DirTree -> "dirtree"
      Some PluginZettelData_Links -> "links"
      Some PluginZettelData_Tags -> "tags"
      Some PluginZettelData_NeuronIgnore -> "neuronignore"

lookupPlugins :: [Text] -> PluginRegistry
lookupPlugins = Map.fromList . mapMaybe lookupPlugin
  where
    lookupPlugin :: Text -> Maybe (Some PluginZettelData, Some Plugin)
    lookupPlugin = \case
      "dirtree" -> Just (Some PluginZettelData_DirTree, Some DirTree.plugin)
      "links" -> Just (Some PluginZettelData_Links, Some Links.plugin)
      "tags" -> Just (Some PluginZettelData_Tags, Some Tags.plugin)
      "neuronignore" -> Just (Some PluginZettelData_NeuronIgnore, Some NeuronIgnore.plugin)
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

afterZettelParse :: PluginRegistry -> [(ZettelID, (FilePath, (Text, DMap PluginZettelData Identity)))] -> [ZettelC]
afterZettelParse plugins fs = do
  let h =
        Map.elems plugins <&> \sp ->
          withSome sp $ \p (m, x) -> (m,) $ _plugin_afterZettelParse p m x
  parseZettels (parseMarkdown $ markdownSpec plugins) fs <&> \x ->
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

-- TODO: Use _plugin_* functions directly!
routePluginData :: ZettelGraph -> ZettelC -> DSum PluginZettelData Identity -> DSum PluginZettelRouteData Identity
routePluginData g z = \case
  PluginZettelData_DirTree :=> Identity dirTree ->
    PluginZettelRouteData_DirTree :=> Identity (DirTree.routePluginData g dirTree)
  PluginZettelData_Links :=> Identity x ->
    PluginZettelRouteData_Links :=> Identity (Links.routePluginData g z x)
  PluginZettelData_Tags :=> Identity x ->
    PluginZettelRouteData_Tags :=> Identity (Tags.routePluginData g z x)
  PluginZettelData_NeuronIgnore :=> Identity () ->
    PluginZettelRouteData_NeuronIgnore :=> Identity ()

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
  PluginZettelRouteData_NeuronIgnore :=> Identity () ->
    blank

renderHandleLink ::
  (PandocBuilder t m, PostBuild t m) =>
  DMap PluginZettelRouteData Identity ->
  Text ->
  Maybe (NeuronWebT t m ())
renderHandleLink pluginData x =
  asum $ flip renderHandleLink' x <$> DMap.toList pluginData

renderHandleLink' ::
  (PandocBuilder t m, PostBuild t m) =>
  DSum PluginZettelRouteData Identity ->
  Text ->
  Maybe (NeuronWebT t m ())
renderHandleLink' = \case
  PluginZettelRouteData_DirTree :=> _ ->
    const Nothing
  PluginZettelRouteData_Links :=> Identity x ->
    Links.renderHandleLink x
  PluginZettelRouteData_Tags :=> Identity x ->
    Tags.renderHandleLink x
  PluginZettelRouteData_NeuronIgnore :=> _ ->
    const Nothing

preJsonStrip :: Zettel -> Zettel
preJsonStrip z =
  let pluginData' = flip fmap (DMap.toList $ zettelPluginData z) $ \case
        x@(PluginZettelData_NeuronIgnore :=> Identity ()) ->
          x
        x@(PluginZettelData_DirTree :=> Identity _) ->
          x
        x@(PluginZettelData_Tags :=> Identity _) ->
          x
        PluginZettelData_Links :=> Identity x ->
          PluginZettelData_Links :=> Identity (Links.preJsonStrip x)
   in z {zettelPluginData = DMap.fromList pluginData'}

-- | Compress the graph to save space, by eliminating the unnecessary
-- surrounding context Pandoc blocks.
stripSurroundingContext :: ZettelGraph -> ZettelGraph
stripSurroundingContext =
  Algo.emap (fmap (second $ const mempty)) . Algo.vmap preJsonStrip