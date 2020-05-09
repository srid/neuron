{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Query.View
  ( renderZettelLink,
    buildQueryView,
  )
where

import Control.Monad.Except
import Data.Default
import Data.Dependent.Sum
import qualified Data.Map.Strict as Map
import Data.Some
import Data.TagTree (Tag (..), TagNode (..), constructTag, foldTagTree, tagMatchAny, tagTree)
import qualified Data.Text as T
import Data.Tree
import Lucid
import Lucid.Base (makeAttribute)
import Neuron.Web.Route (Route (..), routeUrlRelWithQuery)
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Query
import Neuron.Zettelkasten.Query.Error (QueryResultError (..))
import Neuron.Zettelkasten.Query.Theme (LinkView (..), ZettelsView (..))
import Neuron.Zettelkasten.Zettel
import Relude
import qualified Rib
import qualified Text.Pandoc.Builder as B
import Text.URI.QQ (queryKey)

-- | Build the Pandoc AST for query results
buildQueryView :: MonadError QueryResultError m => DSum Query Identity -> m (Either B.Inline B.Block)
buildQueryView = \case
  Query_ZettelByID zid _mconn :=> Identity mres ->
    case mres of
      Nothing -> throwError $ QueryResultError_NoSuchZettel zid
      Just target -> do
        -- pure $ renderZettelLink Nothing res
        pure $ Left $ buildZettelLink target
  _q@(Query_ZettelsByTag _pats _mconn view) :=> Identity res -> pure $ Right $ do
    case zettelsViewGroupByTag view of
      False ->
        buildZettelsLinks res
      True ->
        B.Para [B.Str "Zettel Links, grouped TODO"]
  _q@(Query_Tags _) :=> Identity res -> pure $ Right $ do
    -- Render a list of tags
    -- toHtml $ Some q
    renderTagTree $ foldTagTree $ tagTree res
  where
    mkAttr cls kvs =
      ("", words cls, kvs)
    buildZettelLink Zettel {..} =
      -- TODO: not using Rib for ghcjs, but factorizew this
      let zurl = "/" <> zettelIDText zettelID <> ".html"
       in B.Span
            (mkAttr "zettel-link-container" mempty)
            [ B.Span (mkAttr "zettel-link" mempty) $ pure $
                B.Link mempty [B.Str zettelTitle] (zurl, "TODO: No title")
            ]
    buildZettelsLinks _zs =
      B.Para [B.Str "Zettel Links TODO"]

-- | Render the custom view for the given evaluated query
_renderQueryLink :: forall m. (MonadError QueryResultError m) => DSum Query Identity -> m (Html ())
_renderQueryLink = \case
  Query_ZettelByID zid _mconn :=> Identity mres ->
    case mres of
      Nothing -> throwError $ QueryResultError_NoSuchZettel zid
      Just res ->
        pure $ renderZettelLink Nothing res
  q@(Query_ZettelsByTag pats _mconn view) :=> Identity res -> pure $ do
    toHtml $ Some q
    case zettelsViewGroupByTag view of
      False ->
        -- Render a list of links
        renderZettelLinks (zettelsViewLinkView view) res
      True ->
        forM_ (Map.toList $ groupZettelsByTagsMatching pats res) $ \(tag, zettelGrp) -> do
          span_ [class_ "ui basic pointing below grey label"] $ do
            i_ [class_ "tag icon"] mempty
            toHtml $ unTag tag
          renderZettelLinks (zettelsViewLinkView view) zettelGrp
  q@(Query_Tags _) :=> Identity _res -> pure $ do
    -- Render a list of tags
    -- renderTagTree $ foldTagTree $ tagTree res
    toHtml $ Some q
  where
    -- TODO: Instead of doing this here, group the results in runQuery itself.
    groupZettelsByTagsMatching pats matches =
      fmap sortZettelsReverseChronological $ Map.fromListWith (<>) $ flip concatMap matches $ \z ->
        flip concatMap (zettelTags z) $ \t -> [(t, [z]) | tagMatchAny pats t]
    renderZettelLinks :: LinkView -> [Zettel] -> Html ()
    renderZettelLinks ltheme zs =
      ul_ $ do
        forM_ zs $ \z ->
          li_ $ renderZettelLink (Just ltheme) z

-- | Render a link to an individual zettel.
-- TODO: Remove and consolidate with pandoc renderer
renderZettelLink :: forall m. Monad m => Maybe LinkView -> Zettel -> HtmlT m ()
renderZettelLink (fromMaybe def -> LinkView {..}) Zettel {..} = do
  let zurl = Rib.routeUrlRel $ Route_Zettel zettelID
      mextra =
        if linkViewShowDate
          then case zettelDay of
            Just day ->
              Just $ toHtml $ show @Text day
            Nothing ->
              Nothing
          else Nothing
  span_ [class_ "zettel-link-container"] $ do
    forM_ mextra $ \extra ->
      span_ [class_ "extra"] extra
    let linkTooltip =
          if null zettelTags
            then Nothing
            else Just $ "Tags: " <> T.intercalate "; " (unTag <$> zettelTags)
    span_ ([class_ "zettel-link"] <> withTooltip linkTooltip) $ do
      a_ [href_ zurl] $ toHtml zettelTitle
  where
    withTooltip :: Maybe Text -> [Attribute]
    withTooltip = \case
      Nothing -> []
      Just s ->
        [ makeAttribute "data-tooltip" s,
          makeAttribute "data-inverted" "",
          makeAttribute "data-position" "right center"
        ]

-- |  Render a tag tree along with the count of zettels tagged with it
renderTagTree :: Forest (NonEmpty TagNode, Natural) -> B.Block
renderTagTree t =
  B.Div (mkAttr "tag-tree" mempty) $
    renderForest mempty t
  where
    renderForest :: [TagNode] -> Forest (NonEmpty TagNode, Natural) -> [B.Block]
    renderForest ancestors forest =
      pure $ B.BulletList $
        forest <&> \tree ->
          renderTree ancestors tree
    renderTree :: [TagNode] -> Tree (NonEmpty TagNode, Natural) -> [B.Block]
    renderTree ancestors (Node (tagNode, count) children) =
      mconcat
        [ [renderTag ancestors (tagNode, count)],
          renderForest (ancestors <> toList tagNode) $ toList children
        ]
    renderTag :: [TagNode] -> (NonEmpty TagNode, Natural) -> B.Block
    renderTag ancestors (tagNode, count) =
      let Tag tag = constructTag $ maybe tagNode (<> tagNode) $ nonEmpty ancestors
          url = routeUrlRelWithQuery Route_Search [queryKey|tag|] tag
          tit = show count <> " zettels tagged"
       in B.Div (mkAttr "node" mempty) $ pure $ B.Plain $ pure $
            B.Link
              (mkAttr (bool "" "inactive" $ count == 0) mempty)
              [B.Str $ renderTagNode tagNode]
              (url, tit)
    renderTagNode :: NonEmpty TagNode -> Text
    renderTagNode = \case
      n :| (nonEmpty -> mrest) ->
        case mrest of
          Nothing ->
            unTagNode n
          Just rest ->
            unTagNode n <> "/" <> renderTagNode rest
    mkAttr cls kvs =
      ("", words cls, kvs)
