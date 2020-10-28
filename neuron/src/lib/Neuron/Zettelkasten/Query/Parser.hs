{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Query.Parser
  ( queryFromURI,
    queryFromURILink,
    -- TODO: move to new module
    queryZURILinks,
    ZURILink (..),
  )
where

import Control.Monad.Except
import Data.Some
import Data.TagTree (TagNode (..), TagPattern, constructTag, mkTagPattern)
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Query.Theme
import Neuron.Zettelkasten.Zettel (ZettelQuery (..))
import Relude
import Text.Pandoc.Definition (Inline (Link, Str), Pandoc)
import qualified Text.Pandoc.Walk as W
import Text.URI (URI)
import qualified Text.URI as URI
import Text.URI.QQ (queryKey)
import Text.URI.Util (getQueryParam, hasQueryFlag)

-- | A link to z: URI
data ZURILink = ZURILink
  { -- | This is set to Nothing for autolinks
    _zUriLink_inner :: Maybe [Inline],
    _zUriLink_uri :: URI
  }
  deriving (Eq, Show, Ord)

queryZURILinks :: Pandoc -> [ZURILink]
queryZURILinks = W.query go
  where
    go :: Inline -> [ZURILink]
    go = maybeToList . uriLinkFromInline
    uriLinkFromInline :: Inline -> Maybe ZURILink
    uriLinkFromInline = \case
      Link _attr inlines (url, _title) -> do
        uri <- URI.mkURI url
        let inner = do
              guard $ inlines /= [Str url]
              pure inlines
        pure $ ZURILink inner uri
      _ ->
        Nothing

-- | Parse a query if any from a Markdown link
queryFromURILink :: ZURILink -> Maybe (Some ZettelQuery)
queryFromURILink l@ZURILink {..} =
  queryFromURI (defaultConnection l) _zUriLink_uri
  where
    -- The default connection to use if the user has not explicitly specified
    -- one in the query URI.
    defaultConnection :: ZURILink -> Connection
    defaultConnection ZURILink {..} =
      if isNothing _zUriLink_inner
        then Folgezettel -- Autolinks
        -- NOTE: This will need to be changed when we implement `[[foo | some text]]`
        else OrdinaryConnection

-- | Parse a query from the given URI.
queryFromURI :: Connection -> URI -> Maybe (Some ZettelQuery)
queryFromURI defConn uri = do
  (URI.unRText -> "z") <- URI.uriScheme uri
  -- Non-relevant parts of the URI should be empty
  guard $ isNothing $ URI.uriFragment uri
  zPath <- fmap snd (URI.uriPath uri)
  let -- Found "z:" without a trailing slash
      noSlash = URI.uriAuthority uri == Left False
      -- Found "z:/" instead of "z:"
      hasSlash = URI.uriAuthority uri == Left True
      conn = fromMaybe defConn (queryConn uri)
  case zPath of
    -- Parse z:/<id>
    (URI.unRText -> path) :| []
      | hasSlash -> do
        case parseZettelID path of
          Left _ -> empty
          Right zid ->
            pure $ Some $ ZettelQuery_ZettelByID zid conn
    -- Parse z:zettel/<id>
    (URI.unRText -> "zettel") :| [URI.unRText -> path]
      | noSlash -> do
        case parseZettelID path of
          Left _ -> empty
          Right zid ->
            pure $ Some $ ZettelQuery_ZettelByID zid conn
    -- Parse z:zettels?...
    (URI.unRText -> "zettels") :| []
      | noSlash -> do
        pure $ Some $ ZettelQuery_ZettelsByTag (tagPatterns uri "tag") conn (queryView uri)
    -- Parse z:tags?...
    (URI.unRText -> "tags") :| []
      | noSlash -> do
        pure $ Some $ ZettelQuery_Tags (tagPatterns uri "filter")
    -- Parse z:tag/foo
    (URI.unRText -> "tag") :| (nonEmpty . fmap (TagNode . URI.unRText) -> Just tagNodes)
      | noSlash -> do
        pure $ Some $ ZettelQuery_TagZettel (constructTag tagNodes)
    _ -> empty

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

queryView :: URI -> ZettelsView
queryView uri =
  ZettelsView linkView isGrouped
  where
    isTimeline =
      -- linkTheme=withDate is legacy format; timeline is current standard.
      getQueryParam [queryKey|linkTheme|] uri == Just "withDate"
        || hasQueryFlag [queryKey|timeline|] uri
    isGrouped = hasQueryFlag [queryKey|grouped|] uri
    linkView =
      if isTimeline
        then LinkView_ShowDate
        else
          if hasQueryFlag [queryKey|showid|] uri
            then LinkView_ShowID
            else LinkView_Default

queryConn :: URI -> Maybe Connection
queryConn uri =
  if hasQueryFlag [queryKey|cf|] uri
    then Just OrdinaryConnection
    else Nothing
