{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
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
import Neuron.Frontend.Route (NeuronWebT, Route, RouteConfig)
import Neuron.Frontend.Route.Data.Types
import qualified Neuron.Frontend.Route.Data.Types as R
import Neuron.Frontend.Theme (Theme)
import Neuron.Markdown (NeuronSyntaxSpec, parseMarkdown)
import qualified Neuron.Plugin.Plugins.DirTree as DirTree
import qualified Neuron.Plugin.Plugins.Feed as Feed
import qualified Neuron.Plugin.Plugins.Links as Links
import qualified Neuron.Plugin.Plugins.NeuronIgnore as NeuronIgnore
import qualified Neuron.Plugin.Plugins.Tags as Tags
import qualified Neuron.Plugin.Plugins.UpTree as UpTree
import Neuron.Plugin.Type (Plugin (..))
import Neuron.Zettelkasten.Connection (ContextualConnection)
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.ID (Slug, ZettelID)
import Neuron.Zettelkasten.Resolver (ZIDRef)
import Neuron.Zettelkasten.Zettel
  ( MissingZettel,
    PluginZettelData (..),
    Zettel,
    ZettelC,
    ZettelT (zettelPluginData),
  )
import Neuron.Zettelkasten.Zettel.Parser (parseZettels)
import Reflex.Dom.Core hiding (mapMaybe)
import Reflex.Dom.Pandoc
import qualified Reflex.Dom.Pandoc.Document as PR
import Reflex.Dom.Pandoc.Raw (RawBuilder, elPandocRaw)
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
      Some Feed -> "feed"

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
      "feed" -> Just (Some Feed, Some Feed.plugin)
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
          withSome sp $ \p x -> _plugin_afterZettelParse p x
  parseZettels (parseMarkdown $ markdownSpec plugins) fs <&> \x ->
    foldl' (\x1 f -> f x1) x h

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
  Theme ->
  Css
pluginStyles plugins theme =
  C.body ? do
    mconcat $ Map.elems plugins <&> \sp -> withSome sp $ \p -> _plugin_css p theme

-- TODO: Use _plugin_* functions directly for the many functions below.

routePluginData :: RouteConfig t m -> SiteData -> [ZettelC] -> ZettelGraph -> ZettelC -> DSum PluginZettelData Identity -> DSum PluginZettelRouteData Identity
routePluginData routeCfg siteData zs g z = \case
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
  Feed :=> Identity x ->
    PluginZettelRouteData_Feed :=> Identity (Feed.routePluginData routeCfg siteData zs g z x)

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
  (DomBuilder t m, PostBuild t m) =>
  DMap PluginZettelRouteData Identity ->
  Text ->
  Maybe [Inline] ->
  Maybe (NeuronWebT t m ())
renderHandleLink pluginData url mInline =
  asum $ DMap.toList pluginData <&> \x -> renderHandleLink' x url mInline

renderHandleLink' ::
  (DomBuilder t m, PostBuild t m) =>
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

renderZettelHead :: DomBuilder t m => RouteConfig t m -> (SiteData, ZettelData) -> DSum PluginZettelRouteData Identity -> m ()
renderZettelHead routeCfg val = \case
  PluginZettelRouteData_UpTree :=> Identity x ->
    UpTree.renderZettelHead routeCfg val x
  _ ->
    blank

afterRouteWrite ::
  MonadIO m =>
  RouteConfig t m1 ->
  (ZettelData -> Pandoc -> IO ByteString) ->
  DMap Route Identity ->
  Slug ->
  DSum PluginZettelRouteData Identity ->
  m (Either Text [(Text, FilePath, LText)])
afterRouteWrite routeCfg renderZettel allRoutes slug = \case
  PluginZettelRouteData_Feed :=> Identity routeData -> do
    Feed.writeFeed routeCfg renderZettel allRoutes slug routeData
  _ ->
    pure $ Right mempty

preJsonStrip :: Zettel -> Zettel
preJsonStrip z =
  -- We don't care to send internal data outside of neuron.
  z {zettelPluginData = Nothing}

-- | Compress the graph to save space, by eliminating the unnecessary
-- surrounding context Pandoc blocks.
stripSurroundingContext :: ZettelGraph -> ZettelGraph
stripSurroundingContext =
  Algo.emap (fmap (second $ const mempty)) . Algo.vmap preJsonStrip

-- NOTE: The rendering functions below exist in this module, only to avoid
-- cyclic dependency when using `renderHandleLink`

-- | Render a zettel Pandoc content given its zettel data.
elZettel ::
  (DomBuilder t m, RawBuilder m, PostBuild t m, Prerender js t m) =>
  ZettelData ->
  Pandoc ->
  NeuronWebT t m ()
elZettel zData =
  elPandoc (mkReflexDomPandocConfig zData)

mkReflexDomPandocConfig ::
  forall js t m.
  (DomBuilder t m, RawBuilder m, PostBuild t m, Prerender js t m) =>
  ZettelData ->
  Config t (NeuronWebT t m) ()
mkReflexDomPandocConfig x =
  (PR.defaultConfig @t @m)
    { _config_renderLink = \oldRender url _attrs minner -> do
        fromMaybe oldRender $
          renderHandleLink (R.zettelDataPlugin x) url minner,
      _config_renderCode = \_ (_, langs, _) s -> do
        el "pre" $ elClass "code" (mkLangClass langs) $ text s,
      _config_renderRaw = elPandocRaw
    }
  where
    mkLangClass langs =
      -- Tag code block with "foo language-foo" classes, if the user specified
      -- "foo" as the language identifier. This enables external syntax
      -- highlighters to detect the language.
      --
      -- If no language is specified, use "language-none" as the language This
      -- works at least on prism.js,[1] in that - syntax highlighting is turned
      -- off all the while background styling is applied, to be consistent with
      -- code blocks with language set.
      --
      -- [1] https://github.com/PrismJS/prism/pull/2738
      fromMaybe "language-none" $ do
        lang <- head <$> nonEmpty langs
        pure $ lang <> " language-" <> lang
