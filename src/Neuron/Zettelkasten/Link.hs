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

import Data.Some
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
    ZLink_QueryZettels Connection LinkTheme (Query [Zettel])
  deriving (Eq, Show)

connectionFromURI :: URI.URI -> Connection
connectionFromURI uri =
  fromMaybe Folgezettel $
    case fmap URI.unRText (URI.uriScheme uri) of
      Just scheme
        | scheme `elem` ["zcf", "zcfquery"] ->
          Just OrdinaryConnection
      _ ->
        Nothing

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
    Just scheme | scheme `elem` ["zquery", "zcfquery"] ->
      case queryFromURI uri of
        Right (Some q@(Query_ZettelsByTag _)) ->
          Just $ ZLink_QueryZettels (connectionFromURI uri) (linkThemeFromURI uri) q
        Right _ ->
          error "Bad query for zquery"
        Left err ->
          error err
    _ -> do
      guard $ linkText == URI.render uri
      zid <- rightToMaybe $ parseZettelID' linkText
      pure $ ZLink_ConnectZettel Folgezettel zid

-- | The connections referenced in a zlink.
zLinkConnections :: ZettelStore -> ZLink -> [(Connection, ZettelID)]
zLinkConnections store = \case
  ZLink_ConnectZettel conn zid ->
    [(conn, zid)]
  ZLink_QueryZettels conn _linkTheme q ->
    (conn,) . zettelID <$> runQuery store q
