{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Special Zettel links in Markdown
module Neuron.Zettelkasten.Link where

import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Link.Theme
import Neuron.Zettelkasten.Markdown (MarkdownLink (..))
import Neuron.Zettelkasten.Query (Query (..), queryFromURI, runQuery)
import Neuron.Zettelkasten.Store
import Neuron.Zettelkasten.Zettel
import Relude
import qualified Text.URI as URI

data LinkAction
  = LinkAction_ConnectZettel Connection ZettelID
  | -- | Render a list (or should it be tree?) of links to queries zettels
    LinkAction_QueryZettels Connection LinkTheme [Query]
  deriving (Eq, Show)

linkActionFromLink :: MarkdownLink -> Maybe LinkAction
linkActionFromLink MarkdownLink {markdownLinkUri = uri, markdownLinkText = linkText} =
  -- NOTE: We should probably drop the 'cf' variants in favour of specifying
  -- the connection type as a query param or something.
  case fmap URI.unRText (URI.uriScheme uri) of
    Just "z" ->
      -- The inner link text is supposed to be the zettel ID
      let zid = parseZettelID linkText
       in Just $ LinkAction_ConnectZettel Folgezettel zid
    Just "zcf" ->
      -- The inner link text is supposed to be the zettel ID
      let zid = parseZettelID linkText
       in Just $ LinkAction_ConnectZettel OrdinaryConnection zid
    Just "zquery" ->
      Just $ LinkAction_QueryZettels Folgezettel (linkThemeFromURI uri) (queryFromURI uri)
    Just "zcfquery" ->
      Just $ LinkAction_QueryZettels OrdinaryConnection (linkThemeFromURI uri) (queryFromURI uri)
    _ -> do
      let uriS = URI.render uri
      guard $ uriS == linkText
      zid <- rightToMaybe $ parseZettelID' uriS
      pure $ LinkAction_ConnectZettel Folgezettel zid

linkActionConnections :: ZettelStore -> LinkAction -> [(Connection, ZettelID)]
linkActionConnections store = \case
  LinkAction_ConnectZettel conn zid ->
    [(conn, zid)]
  LinkAction_QueryZettels conn _linkTheme q ->
    (conn,) . zettelID <$> runQuery store q
