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
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ViewPatterns #-}

module Neuron.Zettelkasten.Query.Parser
  ( queryFromURI,
    queryFromURILink,
  )
where

import Control.Monad.Except
import Data.Some
import Data.TagTree (TagPattern, mkTagPattern)
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Query.Error
import Neuron.Zettelkasten.Query.Theme
import Neuron.Zettelkasten.Zettel (ZettelQuery (..))
import Reflex.Dom.Pandoc (URILink (..))
import Relude
import Text.URI (URI)
import qualified Text.URI as URI
import Text.URI.QQ (queryKey)
import Text.URI.Util (getQueryParam, hasQueryFlag)

-- | Parse a query from the given URI.
--
-- This function is used only in the CLI. For handling links in a Markdown file,
-- your want `queryFromURILink` which allows specifying the link text as well.
queryFromURI :: MonadError QueryParseError m => URI -> m (Maybe (Some ZettelQuery))
queryFromURI =
  parseAutoLinks

queryFromURILink :: MonadError QueryParseError m => URILink -> m (Maybe (Some ZettelQuery))
queryFromURILink uriLink =
  if isAutoLink uriLink
    then parseAutoLinks (_uriLink_uri uriLink)
    else parseLegacyLinks uriLink
  where
    isAutoLink (URILink linkText uri) =
      linkText == URI.render uri

-- | Parse commonmark autolink style links, eg: `<2014533>`
parseAutoLinks :: MonadError QueryParseError m => URI -> m (Maybe (Some ZettelQuery))
parseAutoLinks uri = do
  -- Non-relevant parts of the URI should be empty
  if (isJust $ URI.uriFragment uri)
    then pure Nothing
    else do
      let mconn =
            if hasQueryFlag [queryKey|cf|] uri
              then Just OrdinaryConnection
              else Nothing
          -- Found "z:" without a trailing slash
          noSlash = URI.uriAuthority uri == Left False
          -- Found "z:/" instead of "z:"
          hasSlash = URI.uriAuthority uri == Left True
      case fmap URI.unRText (URI.uriScheme uri) of
        Just "z" -> do
          case fmap snd (URI.uriPath uri) of
            -- Parse z:/<id>
            Just ((URI.unRText -> path) :| [])
              | hasSlash -> do
                zid <- parseQueryZettelID uri path
                pure $ Just $ Some $ ZettelQuery_ZettelByID zid mconn
            -- Parse z:zettel/<id>
            Just ((URI.unRText -> "zettel") :| [URI.unRText -> path])
              | noSlash -> do
                zid <- parseQueryZettelID uri path
                pure $ Just $ Some $ ZettelQuery_ZettelByID zid mconn
            -- Parse z:zettels?...
            Just ((URI.unRText -> "zettels") :| [])
              | noSlash -> pure $ do
                pure $ Some $ ZettelQuery_ZettelsByTag (tagPatterns uri "tag") mconn (queryView uri)
            -- Parse z:tags?...
            Just ((URI.unRText -> "tags") :| [])
              | noSlash -> pure $ do
                pure $ Some $ ZettelQuery_Tags (tagPatterns uri "filter")
            _ -> pure Nothing
        Just _otherProto ->
          pure Nothing
        -- The URI has no scheme. We expect this to be the link ID with params.
        Nothing -> pure $ do
          -- Alias to short links
          fmap snd (URI.uriPath uri) >>= \case
            (URI.unRText -> path) :| [] -> do
              zid <- rightToMaybe $ parseZettelID' path
              pure $ Some $ ZettelQuery_ZettelByID zid mconn
            _ ->
              -- Multiple path elements, not supported
              Nothing

-- | Parse legacy style links, eg: `[2014533](z:/)`
parseLegacyLinks :: MonadError QueryParseError m => URILink -> m (Maybe (Some ZettelQuery))
parseLegacyLinks (URILink linkText uri) = do
  case fmap URI.unRText (URI.uriScheme uri) of
    -- Legacy links
    Just proto | proto `elem` ["z", "zcf"] -> do
      zid <- parseQueryZettelID uri linkText
      let mconn = if proto == "zcf" then Just OrdinaryConnection else Nothing
      pure $ Just $ Some $ ZettelQuery_ZettelByID zid mconn
    -- Legacy links
    Just proto | proto `elem` ["zquery", "zcfquery"] ->
      case uriHost uri of
        Right "search" -> do
          let mconn = if proto == "zcfquery" then Just OrdinaryConnection else Nothing
          pure $
            Just $
              Some $
                ZettelQuery_ZettelsByTag (tagPatterns uri "tag") mconn (queryView uri)
        Right "tags" ->
          pure $ Just $ Some $ ZettelQuery_Tags (tagPatterns uri "filter")
        _ ->
          throwError $ QueryParseError_UnsupportedHost uri
    _ ->
      pure Nothing
  where
    uriHost :: URI -> Either Bool Text
    uriHost u =
      fmap (URI.unRText . URI.authHost) (URI.uriAuthority u)

parseQueryZettelID :: MonadError QueryParseError m => URI -> Text -> m ZettelID
parseQueryZettelID uri s =
  liftEither $ first (QueryParseError_InvalidID uri) $ parseZettelID' s

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
  let isTimeline =
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
   in ZettelsView linkView isGrouped
