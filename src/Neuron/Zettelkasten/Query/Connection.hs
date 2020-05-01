{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Query.Connection where

import Neuron.Zettelkasten.Connection
import Relude
import Text.MMark.MarkdownLink
import qualified Text.URI as URI
import Text.URI.QQ (queryKey)
import Text.URI.Util (hasQueryFlag)

connectionFromMarkdownLink :: MarkdownLink -> Maybe Connection
connectionFromMarkdownLink MarkdownLink {markdownLinkUri = uri, markdownLinkText = linkText} =
  case fmap URI.unRText (URI.uriScheme uri) of
    Just scheme
      | scheme `elem` ["zcf", "zcfquery"] ->
        Just OrdinaryConnection
    _ -> do
      -- Try short links (see Query.hs:queryFromMarkdownLink; we need to consolidate these)
      guard $ URI.render uri == linkText
      guard
        `mapM_` [ URI.uriScheme uri == Nothing,
                  URI.uriAuthority uri == Left False,
                  URI.uriFragment uri == Nothing
                ]
      if (hasQueryFlag [queryKey|cf|] uri)
        then pure OrdinaryConnection
        else Nothing
