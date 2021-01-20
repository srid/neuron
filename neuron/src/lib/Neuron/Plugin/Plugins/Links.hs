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
    renderZettelLink,
    renderZettelLinkIDOnly,
    renderPanel,
    preJsonStrip,
  )
where

import Clay (Css, em, (?))
import qualified Clay as C
import qualified Commonmark as CM
import qualified Commonmark.Inlines as CM
import Commonmark.TokParsers (noneOfToks, symbol)
import Control.Monad.Writer
import qualified Data.Dependent.Map as DMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Some
import Data.Tagged (Tagged (Tagged), untag)
import qualified Data.Text as T
import Neuron.Frontend.Route (NeuronWebT, Route (Route_Zettel), neuronRouteLink)
import Neuron.Frontend.Route.Data.Types (LinksData (..))
import Neuron.Frontend.Widget (elNoSnippetSpan, elTime)
import Neuron.Plugin.Type (Plugin (..))
import Neuron.Zettelkasten.Connection
  ( Connection (Folgezettel, OrdinaryConnection),
    ContextualConnection,
  )
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.ID (Slug, ZettelID (unZettelID), getZettelID)
import Neuron.Zettelkasten.Zettel
import qualified Neuron.Zettelkasten.Zettel as Z
import Reflex.Dom.Core hiding (count, mapMaybe, tag)
import Reflex.Dom.Pandoc (PandocBuilder, elPandocInlines)
import Relude hiding (trace, traceShow, traceShowId)
import qualified Text.Megaparsec as M
import Text.Pandoc.Builder (Pandoc (Pandoc))
import Text.Pandoc.Definition (Block, Inline)
import qualified Text.Pandoc.LinkContext as LC
import qualified Text.Pandoc.Util as P
import qualified Text.Parsec as P

-- Directory zettels using this plugin are associated with a `Tag` that
-- corresponds to the directory contents.
plugin :: Plugin LinksData
plugin =
  def
    { _plugin_markdownSpec = wikiLinkSpec,
      _plugin_afterZettelParse = second . const parseLinks,
      _plugin_graphConnections = queryConnections,
      _plugin_renderHandleLink = renderHandleLink,
      _plugin_css = zettelLinkCss
    }

parseLinks :: ZettelT Pandoc -> ZettelT Pandoc
parseLinks z =
  let xs = extractLinkswithContext (zettelContent z)
   in z {zettelPluginData = DMap.insert Links (Identity xs) (zettelPluginData z)}
  where
    extractLinkswithContext doc =
      mapMaybe (uncurry parseQueryLinkWithContext) $
        Map.toList $ LC.queryLinksWithContext doc
      where
        parseQueryLinkWithContext url (attrs, ctx) = do
          (,ctx) <$> parseQueryLink attrs url

renderPanel ::
  forall t m.
  (DomBuilder t m, PostBuild t m) =>
  (Pandoc -> NeuronWebT t m ()) ->
  LinksData ->
  NeuronWebT t m ()
renderPanel elNeuronPandoc LinksData {..} = do
  whenNotNull linksDataBacklinks $ \backlinks -> do
    elClass "nav" "ui attached segment deemphasized backlinksPane" $ do
      renderBacklinks backlinks
  where
    renderBacklinks ::
      (DomBuilder t m, PostBuild t m) =>
      NonEmpty (ContextualConnection, Zettel) ->
      NeuronWebT t m ()
    renderBacklinks links = do
      elClass "h3" "ui header" $ text "Backlinks"
      elClass "ul" "backlinks" $ do
        forM_ links $ \((conn, ctxList), zl) ->
          el "li" $ do
            renderZettelLink Nothing (Just conn) def zl
            elAttr "ul" ("class" =: "context-list" <> "style" =: "zoom: 85%;") $ do
              forM_ ctxList $ \ctx -> do
                elClass "li" "item" $ do
                  void $ elNeuronPandoc $ Pandoc mempty [ctx]

routePluginData :: ZettelGraph -> ZettelC -> [((ZettelID, Connection), [Block])] -> LinksData
routePluginData g z _qs =
  let noteUrls = either (const []) (P.getLinks . zettelContent) z
      backlinks = G.backlinks isJust (sansContent z) g
      backlinksUrls = P.getLinks `concatMap` fmap (snd . fst) backlinks
      allUrls = Set.toList $ Set.fromList $ noteUrls <> backlinksUrls
      linkCache = buildQueryUrlCache (G.getZettels g) allUrls
   in LinksData linkCache backlinks

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

preJsonStrip :: [((ZettelID, Connection), [Block])] -> [((ZettelID, Connection), [Block])]
preJsonStrip conns =
  -- Discard surrounding context
  conns <&> second (const empty)

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
  case DMap.lookup Links zettelPluginData of
    Nothing -> pure mempty
    Just (Identity xs) -> do
      zs <- ask
      fmap concat $
        forM xs $ \((zid, conn), ctx) -> do
          case find ((== zid) . Z.zettelID) zs of
            Nothing -> do
              tell $ one $ Tagged zid
              pure mempty
            Just z2 -> do
              pure [((conn, ctx), z2)]

-- UI
-- --

renderHandleLink :: forall t m. (PandocBuilder t m, PostBuild t m) => LinksData -> Text -> Maybe [Inline] -> Maybe (NeuronWebT t m ())
renderHandleLink LinksData {..} url mInline = do
  r <- Map.lookup url linksDataLinkCache
  pure $ renderZettelLinkMay mInline r

renderZettelLinkMay ::
  (PandocBuilder t m, PostBuild t m) => Maybe [Inline] -> Either MissingZettel (Connection, Zettel) -> NeuronWebT t m ()
renderZettelLinkMay minner = \case
  Left (untag -> zid) ->
    renderMissingZettelLink zid
  Right (conn, target) ->
    renderZettelLink (elPandocInlines <$> minner) (Just conn) Nothing target

-- | Render a link to an individual zettel.
renderZettelLink ::
  (DomBuilder t m, PostBuild t m) =>
  -- | Link inner text
  Maybe (m ()) ->
  -- | Connection type to display
  Maybe Connection ->
  -- | Link theme
  Maybe LinkView ->
  Zettel ->
  NeuronWebT t m ()
renderZettelLink mInner conn (fromMaybe def -> linkView) Zettel {..} = do
  let connClass = show <$> conn
      rawClass = const (Just "errors") =<< untag zettelContent
      mextra =
        case linkView of
          LinkView_Default ->
            Nothing
          LinkView_ShowDate ->
            elTime <$> zettelDate
          LinkView_ShowID ->
            Just $ el "tt" $ text $ unZettelID zettelID
      classes :: [Text] = catMaybes $ [Just "zettel-link-container"] <> [connClass, rawClass]
  elClass "span" (T.intercalate " " classes) $ do
    forM_ mextra $ \extra ->
      elNoSnippetSpan ("class" =: "extra monoFont") $ do
        extra
        -- The extra space is so that double clicking on this extra text
        -- doesn't select the title next.
        text " "
    elAttr "span" ("class" =: "zettel-link" <> maybe mempty ("title" =:) linkTooltip) $ do
      let linkInnerHtml = fromMaybe (text zettelTitle) mInner
      neuronRouteLink (Some $ Route_Zettel zettelSlug) mempty linkInnerHtml
      elConnSuffix conn
  where
    -- If there is custom inner text, put zettel title in tooltip.
    linkTooltip
      | isJust mInner = Just $ "Zettel: " <> zettelTitle
      | otherwise = Nothing
    elConnSuffix :: DomBuilder t m => Maybe Connection -> m ()
    elConnSuffix mconn =
      case mconn of
        Just Folgezettel -> elNoSnippetSpan mempty $ do
          elAttr "sup" ("title" =: "Branching link (folgezettel)") $ text "ᛦ"
        _ -> pure mempty

-- TODO: Eventually refactor this function to reuse what's in renderZettelLink
renderMissingZettelLink :: DomBuilder t m => ZettelID -> m ()
renderMissingZettelLink zid = do
  let classes = ["zettel-link-container", "errors"]
  elClass "span" (T.intercalate " " classes) $ do
    let errMsg = "Wiki-link does not refer to any existing zettel"
    elAttr "span" ("class" =: "zettel-link" <> "title" =: errMsg) $ do
      elAttr "a" mempty $ text $ unZettelID zid

-- | Like `renderZettelLink` but when we only have ID in hand.
renderZettelLinkIDOnly :: (DomBuilder t m, PostBuild t m) => ZettelID -> Slug -> NeuronWebT t m ()
renderZettelLinkIDOnly zid slug =
  elClass "span" "zettel-link-container" $ do
    elClass "span" "zettel-link" $ do
      neuronRouteLink (Some $ Route_Zettel slug) mempty $ text $ unZettelID zid

zettelLinkCss :: Css
zettelLinkCss = do
  "span.zettel-link-container span.zettel-link a" ? do
    C.fontWeight C.bold
    C.textDecoration C.none
  "span.zettel-link-container span.extra" ? do
    C.color C.auto
  "span.zettel-link-container.errors" ? do
    C.border C.solid (C.px 1) C.red
  "[data-tooltip]:after" ? do
    C.fontSize $ em 0.7

-- Markdown parsing
-- ----------------

wikiLinkSpec ::
  (Monad m, CM.IsBlock il bl, CM.IsInline il) =>
  CM.SyntaxSpec m il bl
wikiLinkSpec =
  mempty
    { CM.syntaxInlineParsers = [pLink]
    }
  where
    pLink ::
      (Monad m, CM.IsInline il) =>
      CM.InlineParser m il
    pLink =
      P.try $
        P.choice
          [ -- Folgezettel link: [[[...]]]
            cmAutoLink Folgezettel <$> P.try (wikiLinkP 3),
            -- Cf link: [[...]]
            cmAutoLink OrdinaryConnection <$> P.try (wikiLinkP 2)
          ]
    wikiLinkP :: Monad m => Int -> P.ParsecT [CM.Tok] s m Text
    wikiLinkP n = do
      void $ M.count n $ symbol '['
      s <- fmap CM.untokenize $ some $ noneOfToks [CM.Symbol ']', CM.Symbol '[', CM.LineEnd]
      void $ M.count n $ symbol ']'
      pure s
    cmAutoLink :: CM.IsInline a => Connection -> Text -> a
    cmAutoLink conn url =
      CM.link url title $ CM.str url
      where
        -- Store connetion type in 'title' attribute
        -- TODO: Put it in attrs instead; requires PR to commonmark
        title = show conn
