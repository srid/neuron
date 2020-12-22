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
  ( parseQueryLink,
  )
where

import Data.Default (Default (def))
import Data.Some (Some (..))
import Data.TagTree (TagNode (..), TagPattern, constructTag, mkTagPattern)
import Neuron.Zettelkasten.Connection (Connection (..))
import Neuron.Zettelkasten.ID (getZettelID, parseZettelID)
import Neuron.Zettelkasten.Query.Theme (LinkView (..), ZettelsView (..))
import Neuron.Zettelkasten.Zettel (ZettelQuery (..))
import Relude
import Text.URI (URI)
import qualified Text.URI as URI
import Text.URI.QQ (queryKey)
import Text.URI.Util (getQueryParam, hasQueryFlag)

-- | Parse a query if any from a Markdown link
parseQueryLink :: URI -> Maybe (Some ZettelQuery)
parseQueryLink uri =
  case bareFileUrlPath uri of
    Just path -> do
      -- Allow raw filename (ending with ".md").
      zid <- getZettelID (toString path)
      pure $ Some $ ZettelQuery_ZettelByID zid def
    Nothing -> do
      (URI.unRText -> "z") <- URI.uriScheme uri
      -- Non-relevant parts of the URI should be empty
      guard $ isNothing $ URI.uriFragment uri
      zPath <- fmap snd (URI.uriPath uri)
      let -- Found "z:" without a trailing slash
          noSlash = URI.uriAuthority uri == Left False
          -- Found "z:/" instead of "z:"
          hasSlash = URI.uriAuthority uri == Left True
          conn = fromMaybe def (queryConn uri)
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

queryConn :: URI -> Maybe Connection
queryConn uri =
  if getQueryParam [queryKey|type|] uri == Just "branch"
    then Just Folgezettel
    else Nothing
