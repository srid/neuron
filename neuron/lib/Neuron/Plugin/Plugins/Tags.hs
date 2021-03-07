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
    getZettelTags,
    zettelsByTag,
    appendTags,
  )
where

import Clay (Css, (?))
import qualified Clay as C
import qualified Commonmark as CM
import qualified Commonmark.Inlines as CM
import Commonmark.TokParsers (noneOfToks, symbol)
import Commonmark.Tokens
  ( TokType (LineEnd, Spaces, Symbol, UnicodeSpace),
  )
import Control.Monad.Writer (MonadWriter)
import Data.Dependent.Sum (DSum (..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Some (Some (..), withSome)
import Data.TagTree (Tag (..), TagNode (..), TagPattern (..))
import qualified Data.TagTree as Tag
import qualified Data.TagTree as TagTree
import qualified Data.Text as T
import Data.Tree (Forest, Tree (Node))
import GHC.Natural (naturalToInt)
import Neuron.Frontend.Route (NeuronWebT)
import Neuron.Frontend.Route.Data.Types (TagQueryCache)
import Neuron.Frontend.Widget (semanticIcon)
import Neuron.Markdown (insertZettelMeta, lookupZettelMeta)
import qualified Neuron.Plugin.Plugins.Links as Links
import Neuron.Plugin.Type (Plugin (..))
import Neuron.Zettelkasten.Connection
  ( Connection (..),
    ContextualConnection,
  )
import qualified Neuron.Zettelkasten.Graph as G
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Neuron.Zettelkasten.Zettel
import Reflex.Dom.Core hiding (count, mapMaybe, tag)
import Relude
import Text.Pandoc.Definition (Inline, Pandoc)
import qualified Text.Pandoc.Util as Pandoc
import qualified Text.Parsec as P
import Text.URI (URI)
import qualified Text.URI as URI
import Text.URI.QQ (queryKey, scheme)
import Text.URI.Util (getQueryParam, hasQueryFlag)

-- Directory zettels using this plugin are associated with a `Tag` that
-- corresponds to the directory contents.
plugin :: Plugin TagQueryCache
plugin =
  def
    { _plugin_markdownSpec = inlineTagSpec,
      _plugin_afterZettelParse = second parseTagQuerys,
      _plugin_graphConnections = queryConnections,
      _plugin_renderHandleLink = renderHandleLink,
      _plugin_css = const style
    }

routePluginData :: ZettelGraph -> ZettelC -> [Some TagQuery] -> TagQueryCache
routePluginData g z _qs =
  let allUrls =
        Set.toList . Set.fromList $
          either (const []) (Pandoc.getLinks . zettelContent) z
   in buildTagQueryCache (G.getZettels g) allUrls
  where
    buildTagQueryCache :: [Zettel] -> [([(Text, Text)], Text)] -> TagQueryCache
    buildTagQueryCache zs urlsWithAttrs =
      Map.fromList $
        catMaybes $
          urlsWithAttrs <&> \(attrs, url) -> do
            parseQueryLink attrs url >>= \someQ -> do
              res <- flip runReaderT zs $ runSomeTagQuery someQ
              pure (url, res)

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
queryConnections z = do
  case lookupPluginData Tags z of
    Nothing -> pure mempty
    Just qs -> do
      fmap concat $
        forM qs $ \someQ -> do
          qRes <- runSomeTagQuery someQ
          links <- getConnections qRes
          pure $ first (,mempty) <$> links
  where
    getConnections :: DSum TagQuery Identity -> m [(Connection, Zettel)]
    getConnections = \case
      TagQuery_ZettelsByTag _ conn _mview :=> Identity res ->
        pure $ (conn,) <$> res
      TagQuery_Tags _ :=> _ ->
        pure mempty
      TagQuery_TagZettel _ :=> _ ->
        pure mempty

runSomeTagQuery ::
  ( MonadReader [Zettel] m
  ) =>
  Some TagQuery ->
  m (DSum TagQuery Identity)
runSomeTagQuery someQ =
  withSome someQ $ \q -> do
    zs <- ask
    let res = runTagQuery zs q
    pure $ q :=> Identity res
  where
    runTagQuery :: [Zettel] -> TagQuery r -> r
    runTagQuery zs = \case
      TagQuery_ZettelsByTag pats _mconn (ZettelsView linkView _ _) ->
        let res = zettelsByTag getZettelTags zs pats
         in if linkView == LinkView_ShowDate
              then -- Filter out zettels without a date.
              fforMaybe res $ \z@Zettel {..} -> guard (isJust zettelDate) >> pure z
              else res
      TagQuery_Tags pats ->
        Map.filterWithKey (const . flip TagTree.matchTagQuery pats) allTags
      TagQuery_TagZettel _tag ->
        ()
      where
        allTags :: Map.Map Tag Natural
        allTags =
          Map.fromListWith (+) $
            concatMap (\z -> (,1) <$> toList (getZettelTags z)) zs

zettelsByTag :: (Zettel -> Set Tag) -> [Zettel] -> TagTree.Query -> [Zettel]
zettelsByTag getTags zs q =
  sortZettelsReverseChronological $
    flip filter zs $ \(getTags -> tags) ->
      TagTree.matchTagQueryMulti (toList tags) q

getZettelTags :: ZettelT c -> Set Tag
getZettelTags Zettel {..} =
  maybe Set.empty Set.fromList $ do
    lookupZettelMeta "tags" zettelMeta

-- UI
-- --

renderPanel ::
  forall t m.
  (DomBuilder t m, PostBuild t m) =>
  (Pandoc -> NeuronWebT t m ()) ->
  Zettel ->
  TagQueryCache ->
  NeuronWebT t m ()
renderPanel _elNeuronPandoc z _routeData = do
  whenNotNull (Set.toList $ getZettelTags z) $ \tags -> do
    elAttr "nav" ("id" =: "neuron-tags-pane" <> "class" =: "ui attached segment deemphasized bottomPane") $ do
      renderTags tags
  where
    renderTags :: (DomBuilder t m, PostBuild t m) => NonEmpty Tag -> NeuronWebT t m ()
    renderTags tags = do
      el "div" $ do
        forM_ tags $ \t -> do
          -- NOTE(ui): Ideally this should be at the top, not bottom. But putting it at
          -- the top pushes the zettel content down, introducing unnecessary white
          -- space below the title. So we put it at the bottom for now.
          elAttr
            "span"
            ( "class" =: "ui basic label zettel-tag"
                <> "title" =: "Tag"
            )
            $ text $ unTag t

renderHandleLink :: forall t m. (DomBuilder t m, PostBuild t m) => TagQueryCache -> Text -> Maybe [Inline] -> Maybe (NeuronWebT t m ())
renderHandleLink cache url _mInline = do
  r <- Map.lookup url cache
  pure $ renderQueryResult r

renderInlineTag :: (DomBuilder t m, PostBuild t m) => Tag -> Map Text Text -> m () -> NeuronWebT t m ()
renderInlineTag _tag attr w =
  lift $ elAttr "span" attr w

renderQueryResult ::
  (DomBuilder t m, PostBuild t m) => DSum TagQuery Identity -> NeuronWebT t m ()
renderQueryResult = \case
  q@(TagQuery_ZettelsByTag pats conn view) :=> Identity res -> do
    elClass "section" "tag-query-results" $ do
      renderQuery $ Some q
      if zettelsviewGroupByTag view
        then forM_ (Map.toList $ groupZettelsByTagsMatching pats res) $ \(tag, zettelGrp) -> do
          el "section" $ do
            elClass "span" "ui basic pointing below grey label" $ do
              semanticIcon "tag"
              text $ unTag tag
            el "ul" $
              forM_ zettelGrp $ \z ->
                el "li" $
                  Links.renderZettelLink Nothing (Just conn) (Just $ zettelsviewLinkView view) z
        else divClass "ui list" $ do
          let resToDisplay =
                case zettelsviewLimit view of
                  Nothing -> res
                  Just (naturalToInt -> limit) -> take limit res
          forM_ resToDisplay $ \z -> do
            divClass "item" $
              Links.renderZettelLink Nothing (Just conn) (Just $ zettelsviewLinkView view) z
          when (length resToDisplay /= length res) $ do
            divClass "item" $
              elClass "span" "ui grey text" $ do
                text $ "(displaying only " <> show (length resToDisplay) <> " out of " <> show (length res) <> " zettels)"
  q@(TagQuery_Tags _) :=> Identity res -> do
    elClass "section" "tag-query-results" $ do
      renderQuery $ Some q
      renderTagTree $ TagTree.foldTagTree $ TagTree.tagTree res
  TagQuery_TagZettel tag :=> Identity () ->
    renderInlineTag tag mempty $ do
      elClass "code" "inline-tag" $ do
        text "#"
        text $ unTag tag
  where
    -- TODO: Instead of doing this here, group the results in runQuery itself.
    groupZettelsByTagsMatching pats matches =
      fmap sortZettelsReverseChronological $
        Map.fromListWith (<>) $
          flip concatMap matches $ \z ->
            flip concatMap (getZettelTags z) $ \t -> [(t, [z]) | TagTree.matchTagQuery t pats]

renderQuery :: DomBuilder t m => Some TagQuery -> m ()
renderQuery someQ =
  elAttr "div" ("class" =: "ui horizontal divider" <> "title" =: "Neuron TagQuery") $ do
    case someQ of
      Some (TagQuery_ZettelsByTag q _mconn _mview) -> do
        let qs = show q
            desc = toText $ "Zettels tagged '" <> qs <> "'"
        elAttr "span" ("class" =: "ui basic pointing below black label" <> "title" =: desc) $ do
          semanticIcon "tags"
          text qs
      Some (TagQuery_Tags q) -> do
        let qs = show q
        text $ "Tags matching '" <> qs <> "'"
      Some (TagQuery_TagZettel _tag) -> do
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
      let tag = TagTree.constructTag $ maybe tagNode (<> tagNode) $ nonEmpty ancestors
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

style :: Css
style = do
  "div.tag-tree" ? do
    "div.node" ? do
      C.fontWeight C.bold
      "a.inactive" ? do
        C.color "#555"

-- Parser
-- ------

appendTags :: Set Tag -> ZettelT c -> ZettelT c
appendTags tags z =
  let currentTags = maybe Set.empty Set.fromList $ lookupZettelMeta "tags" (zettelMeta z)
   in z {zettelMeta = insertZettelMeta "tags" (toList $ currentTags <> tags) (zettelMeta z)}

parseTagQuerys :: ZettelT Pandoc -> ZettelT Pandoc
parseTagQuerys z =
  let allUrls =
        Set.toList . Set.fromList $
          Pandoc.getLinks $ zettelContent z
      tagLinks =
        catMaybes $
          allUrls <&> \(attrs, url) -> do
            parseQueryLink attrs url
      inlineTags :: Set Tag = Set.fromList $
        flip fmapMaybe tagLinks $ \case
          Some (TagQuery_TagZettel t) -> Just t
          _ -> Nothing
   in setPluginData Tags tagLinks $ appendTags inlineTags z

-- | Parse a query if any from a Markdown link
parseQueryLink :: [(Text, Text)] -> Text -> Maybe (Some TagQuery)
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
        pure $ Some $ TagQuery_ZettelsByTag (Tag.mkDefaultTagQuery $ tagPatterns uri "tag") conn (queryView uri)
    -- Parse z:tags?...
    (URI.unRText -> "tags") :| []
      | noSlash -> do
        pure $ Some $ TagQuery_Tags (Tag.mkDefaultTagQuery $ tagPatterns uri "filter")
    -- Parse z:tag/foo
    (URI.unRText -> "tag") :| (nonEmpty . fmap (TagNode . URI.unRText) -> Just tagNodes)
      | noSlash -> do
        pure $ Some $ TagQuery_TagZettel (TagTree.constructTag tagNodes)
    _ -> empty
  where
    tagPatterns :: URI -> Text -> [TagPattern]
    tagPatterns uri k =
      TagTree.mkTagPattern <$> getParamValues uri
      where
        getParamValues :: URI -> [Text]
        getParamValues u =
          flip mapMaybe (URI.uriQuery u) $ \case
            URI.QueryParam (URI.unRText -> key) (URI.unRText -> val) ->
              if key == k
                then Just val
                else Nothing
            _ -> Nothing

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
