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
  ( renderQueryLink,
    renderZettelLink,
  )
where

import Data.Dependent.Sum
import qualified Data.Map.Strict as Map
import Data.Some
import Data.TagTree (Tag (..), TagNode (..), constructTag, foldTagTree, tagMatchAny, tagTree)
import Data.Tree
import Lucid
import Neuron.Web.Route (Route (..), routeUrlRelWithQuery)
import Neuron.Zettelkasten.Query
import Neuron.Zettelkasten.Query.Eval (EvaluatedQuery (..))
import Neuron.Zettelkasten.Query.Theme (LinkView (..), ZettelsView (..))
import Neuron.Zettelkasten.Zettel
import Relude
import qualified Rib
import Text.URI.QQ (queryKey)

-- | Render the custom view for the given evaluated query
renderQueryLink :: DSum Query EvaluatedQuery -> Html ()
renderQueryLink = \case
  Query_ZettelByID _zid :=> EvaluatedQuery {..} ->
    renderZettelLink evaluatedQueryTheme evaluatedQueryResult
  q@(Query_ZettelsByTag pats) :=> EvaluatedQuery {..} -> do
    toHtml $ Some q
    case zettelsViewGroupByTag evaluatedQueryTheme of
      False ->
        -- Render a list of links
        renderZettelLinks (zettelsViewLinkView evaluatedQueryTheme) evaluatedQueryResult
      True ->
        forM_ (Map.toList $ groupZettelsByTagsMatching pats evaluatedQueryResult) $ \(tag, zettelGrp) -> do
          span_ [class_ "ui basic pointing below grey label"] $ do
            i_ [class_ "tag icon"] mempty
            toHtml $ unTag tag
          renderZettelLinks (zettelsViewLinkView evaluatedQueryTheme) zettelGrp
  q@(Query_Tags _) :=> EvaluatedQuery {..} -> do
    -- Render a list of tags
    toHtml $ Some q
    renderTagTree $ foldTagTree $ tagTree evaluatedQueryResult
  where
    -- TODO: Instead of doing this here, group the results in runQuery itself.
    groupZettelsByTagsMatching pats matches =
      fmap sortZettelsReverseChronological $ Map.fromListWith (<>) $ flip concatMap matches $ \z ->
        flip concatMap (zettelTags z) $ \t -> [(t, [z]) | tagMatchAny pats t]
    renderZettelLinks :: LinkView -> [Zettel] -> Html ()
    renderZettelLinks ltheme zs =
      ul_ $ do
        forM_ zs $ \z ->
          li_ $ renderZettelLink ltheme z

-- | Render a link to an individual zettel.
renderZettelLink :: forall m. Monad m => LinkView -> Zettel -> HtmlT m ()
renderZettelLink LinkView {..} Zettel {..} = do
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
    span_ [class_ "zettel-link"] $ do
      a_ [href_ zurl] $ toHtml zettelTitle

-- |  Render a tag tree along with the count of zettels tagged with it
renderTagTree :: forall m. Monad m => Forest (NonEmpty TagNode, Natural) -> HtmlT m ()
renderTagTree t =
  div_ [class_ "tag-tree"] $
    renderForest mempty t
  where
    renderForest :: [TagNode] -> Forest (NonEmpty TagNode, Natural) -> HtmlT m ()
    renderForest ancestors forest =
      ul_ $ do
        forM_ forest $ \tree -> do
          li_ $ renderTree ancestors tree
    renderTree :: [TagNode] -> Tree (NonEmpty TagNode, Natural) -> HtmlT m ()
    renderTree ancestors (Node (tagNode, count) children) = do
      renderTag ancestors (tagNode, count)
      whenNotNull children $
        renderForest (ancestors <> toList tagNode) . toList
    renderTag :: [TagNode] -> (NonEmpty TagNode, Natural) -> HtmlT m ()
    renderTag ancestors (tagNode, count) = do
      div_ [class_ "node"] $ do
        let Tag tag = constructTag $ maybe tagNode (<> tagNode) $ nonEmpty ancestors
            attrs = case count of
              0 ->
                [class_ "inactive"]
              _ ->
                [ href_ $ routeUrlRelWithQuery Route_Search [queryKey|tag|] tag,
                  title_ $ show count <> " zettels tagged"
                ]
        a_ attrs $ renderTagNode tagNode
    renderTagNode :: NonEmpty TagNode -> HtmlT m ()
    renderTagNode = \case
      n :| (nonEmpty -> mrest) ->
        case mrest of
          Nothing ->
            toHtml $ unTagNode n
          Just rest -> do
            toHtml $ unTagNode n
            "/"
            renderTagNode rest
