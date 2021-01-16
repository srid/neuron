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

module Neuron.Plugin.Plugins.Tags
  ( plugin,
    routePluginData,
    renderHandleLink,
    renderPanel,
    -- TODO: Nope! Shouldn't complecting with Impulse
    getZettelTags,
    -- TODO: why expose
    zettelsByTag,
  )
where

import qualified Commonmark as CM
import qualified Commonmark.Inlines as CM
import Commonmark.TokParsers (noneOfToks, symbol)
import Commonmark.Tokens
import Control.Monad.Writer
import Data.Default
import qualified Data.Dependent.Map as DMap
import Data.Dependent.Sum (DSum (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Some (Some (..), withSome)
import Data.TagTree
import qualified Data.TagTree as Tag
import qualified Data.Text as T
import Data.Tree (Forest, Tree (Node))
import Data.YAML ((.:?))
import qualified Data.YAML as Y
import GHC.Natural (naturalToInt)
import qualified Neuron.Frontend.Query.View as Q
import Neuron.Frontend.Route
import qualified Neuron.Frontend.Route as R
import Neuron.Frontend.Route.Data.Types (TagQueryLinkCache)
import Neuron.Frontend.Widget (semanticIcon)
import qualified Neuron.Markdown as M
import Neuron.Plugin.Type (Plugin (..))
import Neuron.Zettelkasten.Connection
  ( Connection (Folgezettel, OrdinaryConnection),
    ContextualConnection,
  )
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.Zettel
import Reflex.Dom.Core hiding (count, mapMaybe, tag)
import Reflex.Dom.Pandoc (PandocBuilder)
import Relude hiding (trace, traceShow, traceShowId)
import Text.Pandoc.Definition (Pandoc)
import qualified Text.Pandoc.Util as Pandoc
import qualified Text.Parsec as P
import Text.URI (URI)
import qualified Text.URI as URI
import Text.URI.QQ (queryKey, scheme)
import Text.URI.Util (getQueryParam, hasQueryFlag)

-- Directory zettels using this plugin are associated with a `Tag` that
-- corresponds to the directory contents.
plugin :: Plugin TagQueryLinkCache
plugin =
  def
    { _plugin_markdownSpec = inlineTagSpec,
      _plugin_afterZettelParse = second . parseTagQueryLinks,
      _plugin_graphConnections = queryConnections,
      _plugin_renderHandleLink = renderHandleLink
    }

parseTagQueryLinks :: Maybe (Y.Node Y.Pos) -> ZettelT Pandoc -> ZettelT Pandoc
parseTagQueryLinks myaml z =
  let allUrls =
        Set.toList . Set.fromList $
          Pandoc.getLinks $ zettelContent z
      tagLinks =
        catMaybes $
          allUrls <&> \(attrs, url) -> do
            parseQueryLink attrs url
      tags = fromRight Set.empty $ case myaml of
        Nothing -> pure Set.empty
        Just yaml -> Set.fromList <$> M.runYamlParser (tagsParser yaml)
      tagsData = ZettelTags tags tagLinks
   in z {zettelPluginData = DMap.insert PluginZettelData_Tags (Identity tagsData) (zettelPluginData z)}

tagsParser :: Y.Node Y.Pos -> Y.Parser [Tag]
tagsParser yaml = do
  flip (Y.withMap @[Tag] "tags") yaml $ \m -> do
    fromMaybe mempty <$> liftA2 (<|>) (m .:? "tags") (m .:? "keywords")

routePluginData :: ZettelGraph -> ZettelC -> ZettelTags -> TagQueryLinkCache
routePluginData g z _qs =
  let allUrls =
        Set.toList . Set.fromList $
          either (const []) (Pandoc.getLinks . zettelContent) z
   in buildTagQueryLinkCache (G.getZettels g) allUrls
  where
    buildTagQueryLinkCache :: [Zettel] -> [([(Text, Text)], Text)] -> TagQueryLinkCache
    buildTagQueryLinkCache zs urlsWithAttrs =
      Map.fromList $
        catMaybes $
          urlsWithAttrs <&> \(attrs, url) -> do
            parseQueryLink attrs url >>= \someQ -> do
              res <- flip runReaderT zs $ runSomeTagQueryLink someQ
              pure (url, res)

-- | Parse a query if any from a Markdown link
-- TODO: queryConn should be read from link attribute!
parseQueryLink :: [(Text, Text)] -> Text -> Maybe (Some TagQueryLink)
parseQueryLink attrs url = do
  let conn = case Map.lookup "title" (Map.fromList attrs) of
        Just s -> if s == show Folgezettel then Folgezettel else def
        _ -> def
  uri <- URI.mkURI url
  (URI.unRText -> "z") <- URI.uriScheme uri
  -- Non-relevant parts of the URI should be empty
  guard $ isNothing $ URI.uriFragment uri
  zPath <- fmap snd (URI.uriPath uri)
  let -- Found "z:" without a trailing slash
      noSlash = URI.uriAuthority uri == Left False
  case zPath of
    -- Parse z:zettels?...
    (URI.unRText -> "zettels") :| []
      | noSlash -> do
        pure $ Some $ TagQueryLink_ZettelsByTag (Tag.mkDefaultTagQuery $ tagPatterns uri "tag") conn (queryView uri)
    -- Parse z:tags?...
    (URI.unRText -> "tags") :| []
      | noSlash -> do
        pure $ Some $ TagQueryLink_Tags (Tag.mkDefaultTagQuery $ tagPatterns uri "filter")
    -- Parse z:tag/foo
    (URI.unRText -> "tag") :| (nonEmpty . fmap (TagNode . URI.unRText) -> Just tagNodes)
      | noSlash -> do
        pure $ Some $ TagQueryLink_TagZettel (constructTag tagNodes)
    _ -> empty
  where
    tagPatterns :: URI -> Text -> [TagPattern]
    tagPatterns uri k =
      mkTagPattern <$> getParamValues uri
      where
        getParamValues :: URI -> [Text]
        getParamValues u =
          flip mapMaybe (URI.uriQuery u) $ \case
            URI.QueryParam (URI.unRText -> key) (URI.unRText -> val) ->
              if key == k
                then Just val
                else Nothing
            _ -> Nothing

    -- TODO: move ZettelsView here
    queryView :: URI -> ZettelsView
    queryView uri =
      ZettelsView linkView isGrouped limit
      where
        isTimeline =
          -- linkTheme=withDate is legacy format; timeline is current standard.
          getQueryParam [queryKey|linkTheme|] uri == Just "withDate"
            || hasQueryFlag [queryKey|timeline|] uri
        isGrouped = hasQueryFlag [queryKey|grouped|] uri
        linkView
          | isTimeline = LinkView_ShowDate
          | hasQueryFlag [queryKey|showid|] uri = LinkView_ShowID
          | otherwise = LinkView_Default
        limit = readMaybe . toString =<< getQueryParam [queryKey|limit|] uri

-- Query evaluation
-- ----------------
-- Query connections in the given zettel
--
-- Tell all errors; query parse errors (as already stored in `Zettel`) as well
-- query result errors.
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
  case DMap.lookup PluginZettelData_Tags zettelPluginData of
    Nothing -> pure mempty
    Just (Identity ZettelTags {..}) -> do
      fmap concat $
        forM zettelTagsQueryLinks $ \someQ -> do
          qRes <- runSomeTagQueryLink someQ
          links <- getConnections qRes
          pure $ first (,mempty) <$> links
  where
    getConnections :: DSum TagQueryLink Identity -> m [(Connection, Zettel)]
    getConnections = \case
      TagQueryLink_ZettelsByTag _ conn _mview :=> Identity res ->
        pure $ (conn,) <$> res
      TagQueryLink_Tags _ :=> _ ->
        pure mempty
      TagQueryLink_TagZettel _ :=> _ ->
        pure mempty

runSomeTagQueryLink ::
  ( MonadReader [Zettel] m
  ) =>
  Some TagQueryLink ->
  m (DSum TagQueryLink Identity)
runSomeTagQueryLink someQ =
  withSome someQ $ \q -> do
    zs <- ask
    let res = runTagQueryLink zs q
    pure $ q :=> Identity res

runTagQueryLink :: [Zettel] -> TagQueryLink r -> r
runTagQueryLink zs = \case
  TagQueryLink_ZettelsByTag pats _mconn _mview ->
    zettelsByTag zs pats
  -- TODO: Remove this constructor, not going to bother with allTags, can implement later.
  TagQueryLink_Tags pats ->
    Map.filterWithKey (const . flip matchTagQuery pats) allTags
  TagQueryLink_TagZettel _tag ->
    ()
  where
    allTags :: Map.Map Tag Natural
    allTags =
      Map.fromListWith (+) $
        concatMap (\z -> (,1) <$> toList (getZettelTags z)) zs

zettelsByTag :: [Zettel] -> TagQuery -> [Zettel]
zettelsByTag zs q =
  sortZettelsReverseChronological $
    flip filter zs $ \(getZettelTags -> tags) ->
      matchTagQueryMulti (toList tags) q

getZettelTags :: ZettelT c -> Set Tag
getZettelTags Zettel {..} =
  fromMaybe Set.empty $ do
    Identity ZettelTags {..} <- DMap.lookup PluginZettelData_Tags zettelPluginData
    pure zettelTagsTagged

-- UI
-- --

renderPanel ::
  forall t m.
  (DomBuilder t m, PostBuild t m) =>
  (Pandoc -> NeuronWebT t m ()) ->
  Zettel ->
  TagQueryLinkCache ->
  NeuronWebT t m ()
renderPanel _elNeuronPandoc z _routeData = do
  whenNotNull (Set.toList $ getZettelTags z) $ \tags -> do
    elClass "nav" "ui attached segment deemphasized bottomPane" $ do
      renderTags tags
  where
    renderTags :: (DomBuilder t m, PostBuild t m) => NonEmpty Tag -> NeuronWebT t m ()
    renderTags tags = do
      el "div" $ do
        forM_ tags $ \t -> do
          -- NOTE(ui): Ideally this should be at the top, not bottom. But putting it at
          -- the top pushes the zettel content down, introducing unnecessary white
          -- space below the title. So we put it at the bottom for now.
          R.neuronRouteLink
            (Some $ Route_Impulse $ Just t)
            ( "class" =: "ui basic label zettel-tag"
                <> "title" =: ("See all zettels tagged '" <> unTag t <> "'")
            )
            $ text $ unTag t

renderHandleLink :: forall t m. (PandocBuilder t m, PostBuild t m) => TagQueryLinkCache -> Text -> Maybe (NeuronWebT t m ())
renderHandleLink cache url = do
  r <- Map.lookup url cache
  pure $ renderQueryResult r

renderInlineTag :: (DomBuilder t m, PostBuild t m) => Tag -> Map Text Text -> m () -> NeuronWebT t m ()
renderInlineTag tag = neuronRouteLink (Some $ Route_Impulse $ Just tag)

renderQueryResult ::
  (PandocBuilder t m, PostBuild t m) => DSum TagQueryLink Identity -> NeuronWebT t m ()
renderQueryResult = \case
  q@(TagQueryLink_ZettelsByTag pats conn view) :=> Identity res -> do
    el "section" $ do
      renderQuery $ Some q
      if zettelsViewGroupByTag view
        then forM_ (Map.toList $ groupZettelsByTagsMatching pats res) $ \(tag, zettelGrp) -> do
          el "section" $ do
            elClass "span" "ui basic pointing below grey label" $ do
              semanticIcon "tag"
              text $ unTag tag
            el "ul" $
              forM_ zettelGrp $ \z ->
                el "li" $
                  Q.renderZettelLink Nothing (Just conn) (Just $ zettelsViewLinkView view) z
        else el "ul" $ do
          let resToDisplay =
                case zettelsViewLimit view of
                  Nothing -> res
                  Just (naturalToInt -> limit) -> take limit res
          forM_ resToDisplay $ \z -> do
            el "li" $
              Q.renderZettelLink Nothing (Just conn) (Just $ zettelsViewLinkView view) z
          when (length resToDisplay /= length res) $ do
            el "li" $
              elClass "span" "ui grey text" $ do
                text $ "(displaying only " <> show (length resToDisplay) <> " out of " <> show (length res) <> " zettels)"
  q@(TagQueryLink_Tags _) :=> Identity res -> do
    el "section" $ do
      renderQuery $ Some q
      renderTagTree $ foldTagTree $ tagTree res
  TagQueryLink_TagZettel tag :=> Identity () ->
    renderInlineTag tag mempty $ do
      text "#"
      text $ unTag tag
  where
    -- TODO: Instead of doing this here, group the results in runQuery itself.
    groupZettelsByTagsMatching pats matches =
      fmap sortZettelsReverseChronological $
        Map.fromListWith (<>) $
          flip concatMap matches $ \z ->
            flip concatMap (getZettelTags z) $ \t -> [(t, [z]) | matchTagQuery t pats]

renderQuery :: DomBuilder t m => Some TagQueryLink -> m ()
renderQuery someQ =
  elAttr "div" ("class" =: "ui horizontal divider" <> "title" =: "Neuron TagQueryLink") $ do
    case someQ of
      Some (TagQueryLink_ZettelsByTag q _mconn _mview) -> do
        let qs = show q
            desc = toText $ "Zettels tagged '" <> qs <> "'"
        elAttr "span" ("class" =: "ui basic pointing below black label" <> "title" =: desc) $ do
          semanticIcon "tags"
          text qs
      Some (TagQueryLink_Tags q) -> do
        let qs = show q
        text $ "Tags matching '" <> qs <> "'"
      Some (TagQueryLink_TagZettel _tag) -> do
        blank

renderTagTree ::
  forall t m.
  (DomBuilder t m, PostBuild t m) =>
  Forest (NonEmpty TagNode, Natural) ->
  NeuronWebT t m ()
renderTagTree t =
  divClass "tag-tree" $
    renderForest mempty t
  where
    renderForest :: [TagNode] -> Forest (NonEmpty TagNode, Natural) -> NeuronWebT t m ()
    renderForest ancestors forest =
      el "ul" $ do
        forM_ forest $ \tree ->
          el "li" $ renderTree ancestors tree
    renderTree :: [TagNode] -> Tree (NonEmpty TagNode, Natural) -> NeuronWebT t m ()
    renderTree ancestors (Node (tagNode, count) children) = do
      renderTag ancestors (tagNode, count)
      renderForest (ancestors <> toList tagNode) $ toList children
    renderTag :: [TagNode] -> (NonEmpty TagNode, Natural) -> NeuronWebT t m ()
    renderTag ancestors (tagNode, count) = do
      let tag = constructTag $ maybe tagNode (<> tagNode) $ nonEmpty ancestors
          tit = show count <> " zettels tagged"
          cls = bool "" "inactive" $ count == 0
      divClass "node" $ do
        renderInlineTag tag ("class" =: cls <> "title" =: tit) $
          text $ renderTagNode tagNode
    renderTagNode :: NonEmpty TagNode -> Text
    renderTagNode = \case
      n :| (nonEmpty -> mrest) ->
        case mrest of
          Nothing ->
            unTagNode n
          Just rest ->
            unTagNode n <> "/" <> renderTagNode rest

-- View
-- ----

-- Parser
-- ------

inlineTagSpec ::
  (Monad m, CM.IsBlock il bl, CM.IsInline il) =>
  CM.SyntaxSpec m il bl
inlineTagSpec =
  mempty
    { CM.syntaxInlineParsers = [pInlineTag]
    }
  where
    pInlineTag ::
      (Monad m, CM.IsInline il) =>
      CM.InlineParser m il
    pInlineTag = P.try $ do
      _ <- symbol '#'
      tag <- CM.untokenize <$> inlineTagP
      case makeZTagURI tag of
        Nothing ->
          fail "Not an inline tag"
        Just (URI.render -> url) ->
          pure $! cmAutoLink OrdinaryConnection url
    makeZTagURI :: Text -> Maybe URI
    makeZTagURI s = do
      tag <- URI.mkPathPiece "tag"
      path <- traverse URI.mkPathPiece $ T.splitOn "/" s
      pure $ URI.URI (Just [scheme|z|]) (Left False) (Just (False, tag :| path)) [] Nothing

-- | Create a commonmark link element
cmAutoLink :: CM.IsInline a => Connection -> Text -> a
cmAutoLink conn url =
  CM.link url title $ CM.str url
  where
    -- Store connetion type in 'title' attribute
    -- TODO: Put it in attrs instead; requires PR to commonmark
    title = show conn

inlineTagP :: Monad m => P.ParsecT [CM.Tok] s m [CM.Tok]
inlineTagP =
  some (noneOfToks $ [Spaces, UnicodeSpace, LineEnd] <> fmap Symbol punctuation)
  where
    punctuation = "[];:,.?!"
