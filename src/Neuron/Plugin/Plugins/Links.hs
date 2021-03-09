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
import Control.Monad.Writer (MonadWriter, tell)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Some (Some (Some))
import Data.Tagged (Tagged (Tagged), untag)
import qualified Data.Text as T
import qualified Data.Time.DateMayTime as DMT
import Neuron.Frontend.Route (NeuronWebT, Route (Route_Zettel), neuronRouteLink)
import Neuron.Frontend.Route.Data.Types (LinksData (..))
import Neuron.Frontend.Theme (Theme)
import qualified Neuron.Frontend.Theme as Theme
import Neuron.Frontend.Widget (elNoSnippetSpan, elTime)
import Neuron.Plugin.Type (Plugin (..))
import Neuron.Zettelkasten.Connection
  ( Connection (..),
    ContextualConnection,
  )
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.ID (Slug, ZettelID (unZettelID), getZettelID)
import Neuron.Zettelkasten.Zettel
import qualified Neuron.Zettelkasten.Zettel as Z
import Reflex.Dom.Core hiding (count, mapMaybe, tag)
import Reflex.Dom.Pandoc (elPandocInlines)
import Relude hiding (trace, traceShow, traceShowId)
import Relude.Extra (groupBy)
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
      _plugin_afterZettelParse = second parseLinks,
      _plugin_graphConnections = queryConnections,
      _plugin_renderHandleLink = renderHandleLink,
      _plugin_css = zettelLinkCss
    }

routePluginData :: ZettelGraph -> ZettelC -> [((ZettelID, Connection), [Block])] -> LinksData
routePluginData g z _qs =
  let noteUrls = either (const []) (P.getLinks . zettelContent) z
      backlinks = G.backlinks isJust (sansContent z) g
      backlinksUrls = P.getLinks `concatMap` fmap (snd . fst) backlinks
      allUrls = Set.toList $ Set.fromList $ noteUrls <> backlinksUrls
      linkCache = buildLinkCache (G.getZettels g) allUrls
      backlinksSorted = sortOn snd backlinks
   in LinksData linkCache backlinksSorted

-- FIXME: This link cache can't be used on backinks, the connection is assumed
-- to be from current zettel, whereas in backlinks, they originate from the
-- individual backlinks. See `renderPanel` which is passed elNeuronPandoc that
-- is passed this (hardcoded) cache.
type LinkCache = Map Text (Either MissingZettel (Connection, Zettel))

buildLinkCache :: [Zettel] -> [([(Text, Text)], Text)] -> LinkCache
buildLinkCache zs urlsWithAttrs =
  Map.fromList $
    catMaybes $
      urlsWithAttrs <&> \(attrs, url) -> do
        (zid, conn) <- parseQueryLink attrs url
        case find ((== zid) . Z.zettelID) zs of
          Nothing -> pure (url, Left (Tagged zid))
          Just z ->
            pure (url, Right (conn, z))

parseLinks :: ZettelT Pandoc -> ZettelT Pandoc
parseLinks z =
  let xs = extractLinkswithContext (zettelContent z)
   in setPluginData Links xs z
  where
    extractLinkswithContext doc =
      concatMap (uncurry parseQueryLinkWithContext) $
        Map.toList $ LC.queryLinksWithContext doc
      where
        parseQueryLinkWithContext url occurs = do
          flip mapMaybe (toList occurs) $ \(attrs, ctx) ->
            (,ctx) <$> parseQueryLink attrs url

parseQueryLink :: [(Text, Text)] -> Text -> Maybe (ZettelID, Connection)
parseQueryLink attrs url = do
  let conn :: Connection =
        fromMaybe def $ readMaybe . toString =<< Map.lookup "title" (Map.fromList attrs)
  path <- asMarkdownPath url
  zid <- getZettelID (toString path)
  pure (zid, conn)
  where
    -- Return .md file path, for the given link text.
    -- Supports "foo" or "foo.md", but not relative paths or URLs.
    asMarkdownPath :: Text -> Maybe Text
    asMarkdownPath s = do
      -- Exclude relative URLs or paths.
      guard $ not $ "/" `T.isInfixOf` s || ":" `T.isInfixOf` s
      -- Add .md extension
      if ".md" `T.isSuffixOf` s
        then pure s
        else pure (s <> ".md")

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
queryConnections z = do
  case lookupPluginData Links z of
    Nothing -> pure mempty
    Just xs -> do
      zs <- ask
      fmap concat $
        forM xs $ \((zid, conn), ctx) -> do
          case find ((== zid) . Z.zettelID) zs of
            Nothing -> do
              -- Linked zettel not found; report as missing.
              tell $ one $ Tagged zid
              pure mempty
            Just z2 -> do
              pure $ one ((conn, ctx), z2)

-- UI
-- --

renderPanel ::
  forall t m.
  (DomBuilder t m, PostBuild t m) =>
  (Pandoc -> NeuronWebT t m ()) ->
  LinksData ->
  NeuronWebT t m ()
renderPanel elNeuronPandoc LinksData {..} = do
  whenNotNull linksDataBacklinks $ \backlinks -> do
    elAttr "nav" ("id" =: "neuron-backlinks-pane" <> "class" =: "ui attached segment deemphasized backlinksPane") $ do
      renderBacklinks backlinks
  where
    renderBacklinks ::
      (DomBuilder t m, PostBuild t m) =>
      NonEmpty (ContextualConnection, Zettel) ->
      NeuronWebT t m ()
    renderBacklinks ungroupedLinks = do
      let grouped :: [(Connection, NonEmpty (ContextualConnection, Zettel))] =
            Map.toList $ groupBy (fst . fst) ungroupedLinks
      forM_ grouped $ \(grpConn, links) -> do
        elClass "h3" "ui header" $ backlinkType grpConn
        elClass "ul" "backlinks" $ do
          forM_ links $ \((conn, ctxList), zl) ->
            el "li" $ do
              renderZettelLink Nothing (Just conn) def zl
              elAttr "ul" ("class" =: "context-list" <> "style" =: "zoom: 85%;") $ do
                forM_ ctxList $ \ctx -> do
                  elClass "li" "item" $ do
                    void $ elNeuronPandoc $ Pandoc mempty [ctx]
    backlinkType = \case
      OrdinaryConnection ->
        text "Backlinks"
      Folgezettel ->
        elAttr "span" ("title" =: "Backlinks from folgezettel parents") $ text "Uplinks"
      FolgezettelInverse ->
        elAttr "span" ("title" =: "Backlinks from folgezettel children") $ text "Downlinks"

renderHandleLink :: forall t m. (DomBuilder t m, PostBuild t m) => LinksData -> Text -> Maybe [Inline] -> Maybe (NeuronWebT t m ())
renderHandleLink LinksData {..} url mInline = do
  r <- Map.lookup url linksDataLinkCache
  pure $ renderZettelLinkMay mInline r

renderZettelLinkMay ::
  (DomBuilder t m, PostBuild t m) => Maybe [Inline] -> Either MissingZettel (Connection, Zettel) -> NeuronWebT t m ()
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
      rawClass = const (Just "errors") =<< zettelContent
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
      elConnFlag conn $ neuronRouteLink (Some $ Route_Zettel zettelSlug) mempty linkInnerHtml
  where
    linkTooltip = do
      case (mInner, zettelDate) of
        -- If using custom link text, put zettel title in tooltip
        (Just _inner, _) -> Just $ "Zettel: " <> zettelTitle
        -- Otherwise use date if any.
        (_, Just dt) -> Just $ DMT.formatDateMayTime dt
        _ -> Nothing
    elConnFlag :: DomBuilder t m => Maybe Connection -> m () -> m ()
    elConnFlag mconn w =
      let folgeFlag =
            elNoSnippetSpan ("title" =: "Folgezettel" <> "style" =: "user-select: none; color: gray") $ text "#"
       in case mconn of
            Just Folgezettel -> do
              w >> folgeFlag
            Just FolgezettelInverse -> do
              folgeFlag >> w
            _ ->
              w

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

zettelLinkCss :: Theme -> Css
zettelLinkCss theme = do
  "span.zettel-link-container span.zettel-link a" ? do
    C.color (Theme.textColor theme)
    C.fontWeight C.bold
    C.textDecoration C.none
  "span.zettel-link-container span.zettel-link a:hover" ? do
    C.backgroundColor (Theme.textBackgroundColor theme)
  "span.zettel-link-container span.extra" ? do
    C.color C.auto
  "span.zettel-link-container.errors" ? do
    C.border C.solid (C.px 1) C.red
  "span.zettel-link-container.errors span.zettel-link a:hover" ? do
    C.important $ C.textDecoration C.none
    C.cursor C.notAllowed
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
          [ -- Folgezettel links
            cmAutoLink Folgezettel <$> P.try (wikiLinkP 3),
            cmAutoLink FolgezettelInverse <$> P.try (symbol '#' *> wikiLinkP 2),
            cmAutoLink Folgezettel <$> P.try (wikiLinkP 2 <* symbol '#'),
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
