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
  ( queryFromPandocLink,
  )
where

import Data.Some (Some (..))
import Data.TagTree (TagNode (..), TagPattern, constructTag, mkTagPattern)
import Neuron.Reader.Type (ZettelFormat (ZettelFormat_Markdown))
import Neuron.Zettelkasten.Connection (Connection (..))
import Neuron.Zettelkasten.ID (getZettelID, parseZettelID)
import Neuron.Zettelkasten.Query.Theme (LinkView (..), ZettelsView (..))
import Neuron.Zettelkasten.Zettel (ZettelQuery (..))
import Relude
import qualified Text.Pandoc.Util as P
import Text.URI (URI)
import qualified Text.URI as URI
import Text.URI.QQ (queryKey)
import Text.URI.Util (getQueryParam, hasQueryFlag)

-- | Parse a query if any from a Markdown link
queryFromPandocLink :: P.PandocLink -> Maybe (Some ZettelQuery)
queryFromPandocLink l@P.PandocLink {..} =
  queryFromURI (defaultConnection l) _pandocLink_uri
  where
    -- The default connection to use if the user has not explicitly specified
    -- one in the query URI.
    -- TODO: Understand and document why the default *still* has to be Folgezettel?
    -- Perhaps remove "?cf" and make "?branch" explicit
    -- Right, because wiki-link parser explicitly puts ?cf, with default being
    -- Folgezettel. We should change that!
    defaultConnection :: P.PandocLink -> Connection
    defaultConnection pl =
      if P.isAutoLink pl
        then Folgezettel -- Autolinks
        -- NOTE: This will need to be changed when we implement `[[foo | some text]]`
        else OrdinaryConnection

-- | Parse a query from the given URI.
queryFromURI :: Connection -> URI -> Maybe (Some ZettelQuery)
queryFromURI defConn uri = do
  case bareFileUrlPath uri of
    Just path -> do
      -- Allow raw filename (ending with ".md"). HACK: hardcoding format, but we
      -- shouldn't.
      zid <- getZettelID ZettelFormat_Markdown (toString path)
      pure $ Some $ ZettelQuery_ZettelByID zid OrdinaryConnection
    Nothing -> do
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
  where
    bareFileUrlPath u = do
      guard $ isNothing $ URI.uriScheme u
      guard $ URI.uriAuthority u == Left False
      (False, path :| []) <- URI.uriPath u
      pure $ URI.unRText path

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
