{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Query.Connection where

import Data.TagTree (Tag)
import Neuron.Zettelkasten.Connection
import Neuron.Zettelkasten.Zettel
import Relude
import Text.MMark.MarkdownLink
import qualified Text.URI as URI
import Text.URI.QQ (queryKey)
import Text.URI.Util (hasQueryFlag)

type family QueryConnection q

type instance QueryConnection (Maybe Zettel) = Connection

type instance QueryConnection [Zettel] = Connection

type instance QueryConnection (Map Tag Natural) = ()

connectionFromMarkdownLink :: MarkdownLink -> Connection
connectionFromMarkdownLink MarkdownLink {markdownLinkUri = uri, markdownLinkText = linkText} =
  fromMaybe Folgezettel $
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
