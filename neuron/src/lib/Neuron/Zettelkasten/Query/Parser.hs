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
import qualified Data.Map.Strict as Map
import Data.Some (Some (..))
import Data.TagTree (TagNode (..), TagPattern, constructTag, mkDefaultTagQuery, mkTagPattern)
import qualified Data.Text as T
import Neuron.Zettelkasten.Connection (Connection (..))
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Query.Theme (LinkView (..), ZettelsView (..))
import Neuron.Zettelkasten.Zettel (ZettelQuery (..))
import Relude hiding (traceShowId)
import Text.URI (URI)
import qualified Text.URI as URI
import Text.URI.QQ (queryKey)
import Text.URI.Util (getQueryParam, hasQueryFlag)

-- | Parse a query if any from a Markdown link
-- TODO: queryConn should be read from link attribute!
parseQueryLink :: [(Text, Text)] -> Text -> Maybe (Either (ZettelID, Connection) (Some ZettelQuery))
parseQueryLink attrs url = do
  let conn = case Map.lookup "title" (Map.fromList attrs) of
        Just s -> if s == show Folgezettel then Folgezettel else def
        _ -> def
  determineLinkType url >>= \case
    Left path -> do
      -- Allow raw filename (ending with ".md").
      zid <- getZettelID (toString path)
      pure $ Left (zid, conn)
    Right uri -> do
      (URI.unRText -> "z") <- URI.uriScheme uri
      -- Non-relevant parts of the URI should be empty
      guard $ isNothing $ URI.uriFragment uri
      zPath <- fmap snd (URI.uriPath uri)
      let -- Found "z:" without a trailing slash
          noSlash = URI.uriAuthority uri == Left False
          -- Found "z:/" instead of "z:"
          hasSlash = URI.uriAuthority uri == Left True
      case zPath of
        -- Parse z:/<id>
        (URI.unRText -> path) :| []
          | hasSlash -> do
            case parseZettelID path of
              Left _ -> empty
              Right zid ->
                pure $ Left (zid, conn)
        -- Parse z:zettel/<id>
        (URI.unRText -> "zettel") :| [URI.unRText -> path]
          | noSlash -> do
            case parseZettelID path of
              Left _ -> empty
              Right zid ->
                pure $ Left (zid, conn)
        -- Parse z:zettels?...
        (URI.unRText -> "zettels") :| []
          | noSlash -> do
            pure $ Right $ Some $ ZettelQuery_ZettelsByTag (mkDefaultTagQuery $ tagPatterns uri "tag") conn (queryView uri)
        -- Parse z:tags?...
        (URI.unRText -> "tags") :| []
          | noSlash -> do
            pure $ Right $ Some $ ZettelQuery_Tags (mkDefaultTagQuery $ tagPatterns uri "filter")
        -- Parse z:tag/foo
        (URI.unRText -> "tag") :| (nonEmpty . fmap (TagNode . URI.unRText) -> Just tagNodes)
          | noSlash -> do
            pure $ Right $ Some $ ZettelQuery_TagZettel (constructTag tagNodes)
        _ -> empty
  where
    -- NOTE: This treats "foo.html" as zettel ref (why shouldn't it?), but not
    -- "./foo.html"
    determineLinkType :: Text -> Maybe (Either Text URI)
    determineLinkType s = do
      if "/" `T.isInfixOf` s || ":" `T.isInfixOf` s
        then Right <$> URI.mkURI s
        else pure $ Left s

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
