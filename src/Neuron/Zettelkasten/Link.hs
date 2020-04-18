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

-- | A ZLink is a special link supported by Neuron
--
-- z:, zcf:, zquery: and zcfquery:
data ZLink
  = ZLink_ConnectZettel Connection ZettelID
  | -- | Render a list (or should it be tree?) of links to queries zettels
    ZLink_QueryZettels Connection LinkTheme [Query]
  deriving (Eq, Show)

mkZLink :: HasCallStack => MarkdownLink -> Maybe ZLink
mkZLink MarkdownLink {markdownLinkUri = uri, markdownLinkText = linkText} =
  -- NOTE: We should probably drop the 'cf' variants in favour of specifying
  -- the connection type as a query param or something.
  case fmap URI.unRText (URI.uriScheme uri) of
    Just "z" ->
      -- The inner link text is supposed to be the zettel ID
      let zid = parseZettelID linkText
       in Just $ ZLink_ConnectZettel Folgezettel zid
    Just "zcf" ->
      -- The inner link text is supposed to be the zettel ID
      let zid = parseZettelID linkText
       in Just $ ZLink_ConnectZettel OrdinaryConnection zid
    Just "zquery" ->
      Just $ ZLink_QueryZettels Folgezettel (linkThemeFromURI uri) (either error id $ queryFromURI uri)
    Just "zcfquery" ->
      Just $ ZLink_QueryZettels OrdinaryConnection (linkThemeFromURI uri) (either error id $ queryFromURI uri)
    _ -> do
      let uriS = URI.render uri
      guard $ uriS == linkText
      zid <- rightToMaybe $ parseZettelID' uriS
      pure $ ZLink_ConnectZettel Folgezettel zid

-- | The connections referenced in a zlink.
zLinkConnections :: ZettelStore -> ZLink -> [(Connection, ZettelID)]
zLinkConnections store = \case
  ZLink_ConnectZettel conn zid ->
    [(conn, zid)]
  ZLink_QueryZettels conn _linkTheme q ->
    (conn,) . zettelID <$> runQuery store q
