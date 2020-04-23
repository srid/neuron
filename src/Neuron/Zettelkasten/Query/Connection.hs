{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Query.Connection where

import Data.TagTree (Tag)
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Zettel
import Relude
import qualified Text.URI as URI

type family QueryConnection q

type instance QueryConnection (Maybe Zettel) = Connection

type instance QueryConnection [Zettel] = Connection

type instance QueryConnection (Map Tag Natural) = ()

connectionFromURI :: URI.URI -> Connection
connectionFromURI uri =
  fromMaybe Folgezettel $
    case fmap URI.unRText (URI.uriScheme uri) of
      Just scheme
        | scheme `elem` ["zcf", "zcfquery"] ->
          Just OrdinaryConnection
      _ ->
        Nothing
