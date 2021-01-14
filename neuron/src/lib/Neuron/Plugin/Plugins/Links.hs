{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Plugin.Plugins.Links
  ( plugin,
    routePluginData,
    renderHandleLink,
    renderPanel,
  )
where

import Control.Monad.Writer
import qualified Data.Dependent.Map as DMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Tagged
import qualified Data.Text as T
import qualified Neuron.Frontend.Query.View as Q
import Neuron.Frontend.Route
import Neuron.Frontend.Route.Data.Types (LinkCache)
import Neuron.Plugin.Type (Plugin (..))
import Neuron.Zettelkasten.Connection
  ( Connection (Folgezettel),
    ContextualConnection,
  )
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Zettel
import qualified Neuron.Zettelkasten.Zettel as Z
import Reflex.Dom.Core hiding (count, mapMaybe, tag)
import Reflex.Dom.Pandoc (PandocBuilder, elPandocInlines)
import Relude hiding (trace, traceShow, traceShowId)
import Text.Pandoc.Definition (Block, Inline, Pandoc)
import qualified Text.Pandoc.LinkContext as LC
import qualified Text.Pandoc.Util as P

-- Directory zettels using this plugin are associated with a `Tag` that
-- corresponds to the directory contents.
plugin :: Plugin LinkCache
plugin =
  def
    { _plugin_afterZettelParse = second parseTagQueryLinks,
      _plugin_graphConnections = queryConnections,
      _plugin_renderHandleLink = renderHandleLink
    }

parseTagQueryLinks :: HasCallStack => ZettelT Pandoc -> ZettelT Pandoc
parseTagQueryLinks z =
  let xs = extractLinkswithContext (zettelContent z)
   in z {zettelPluginData = DMap.insert PluginZettelData_Links (Identity xs) (zettelPluginData z)}
  where
    extractLinkswithContext doc =
      mapMaybe (uncurry parseQueryLinkWithContext) $
        Map.toList $ LC.queryLinksWithContext doc
      where
        parseQueryLinkWithContext url (attrs, ctx) = do
          (,ctx) <$> parseQueryLink attrs url

renderPanel :: (DomBuilder t m, PostBuild t m) => LinkCache -> NeuronWebT t m ()
renderPanel _x = do
  -- TODO: backlinks
  text "TODO Linkjs plugin"

routePluginData :: ZettelGraph -> ZettelC -> [((ZettelID, Connection), [Block])] -> LinkCache
routePluginData g z _qs =
  let allUrls =
        Set.toList . Set.fromList $
          either (const []) (P.getLinks . zettelContent) z
   in buildQueryUrlCache (G.getZettels g) allUrls

type QueryUrlCache = Map Text (Either MissingZettel (Connection, Zettel))

buildQueryUrlCache :: [Zettel] -> [([(Text, Text)], Text)] -> QueryUrlCache
buildQueryUrlCache zs urlsWithAttrs =
  Map.fromList $
    catMaybes $
      urlsWithAttrs <&> \(attrs, url) -> do
        (zid, conn) <- parseQueryLink attrs url
        case find ((== zid) . Z.zettelID) zs of
          Nothing -> pure (url, Left (Tagged zid))
          Just z ->
            pure (url, Right (conn, z))

parseQueryLink :: [(Text, Text)] -> Text -> Maybe (ZettelID, Connection)
parseQueryLink attrs url = do
  let conn = case Map.lookup "title" (Map.fromList attrs) of
        Just s -> if s == show Folgezettel then Folgezettel else def
        _ -> def
  path <- determineLinkType url
  -- Allow raw filename (ending with ".md").
  zid <- getZettelID (toString path)
  pure (zid, conn)
  where
    -- NOTE: This treats "foo.html" as zettel ref (why shouldn't it?), but not
    -- "./foo.html"
    determineLinkType :: Text -> Maybe Text
    determineLinkType s = do
      guard $ not $ "/" `T.isInfixOf` s || ":" `T.isInfixOf` s
      pure s

-- Query evaluation
-- ----------------

queryConnections ::
  forall m.
  ( -- Running queries requires the zettels list.
    MonadReader [Zettel] m,
    -- Track missing zettel links in writer
    MonadWriter [MissingZettel] m
  ) =>
  Zettel ->
  m [(ContextualConnection, Zettel)]
queryConnections Zettel {..} = do
  case DMap.lookup PluginZettelData_Links zettelPluginData of
    Nothing -> pure mempty
    Just (Identity xs) -> do
      zs <- ask
      fmap concat $
        forM xs $ \((zid, conn), ctx) -> do
          case find ((== zid) . Z.zettelID) zs of
            Nothing -> pure mempty
            Just z2 -> do
              pure [((conn, ctx), z2)]

-- UI
-- --

renderHandleLink :: forall t m. (PandocBuilder t m, PostBuild t m) => LinkCache -> Text -> Maybe (NeuronWebT t m ())
renderHandleLink cache url = do
  r <- Map.lookup url cache
  pure $ renderZettelLinkMay Nothing r

renderZettelLinkMay ::
  (PandocBuilder t m, PostBuild t m) => Maybe [Inline] -> Either MissingZettel (Connection, Zettel) -> NeuronWebT t m ()
renderZettelLinkMay minner = \case
  Left (untag -> zid) ->
    Q.renderMissingZettelLink zid
  Right (conn, target) ->
    Q.renderZettelLink (elPandocInlines <$> minner) (Just conn) Nothing target
