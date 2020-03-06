{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Zettel ID
module Neuron.Zettelkasten.Route where

import Neuron.Zettelkasten.Graph
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Store
import Neuron.Zettelkasten.Type
import Path
import Relude
import Rib (IsRoute (..))

data Route store graph a where
  Route_Index :: Route ZettelStore ZettelGraph ()
  Route_Zettel :: ZettelID -> Route ZettelStore ZettelGraph ()

instance IsRoute (Route store graph) where
  routeFile = \case
    Route_Index ->
      pure [relfile|index.html|]
    Route_Zettel (unZettelID -> zid) ->
      parseRelFile $ toString zid <> ".html"

routeName :: Route store graph a -> Text
routeName = \case
  Route_Index -> "Zettels"
  Route_Zettel zid -> unZettelID zid

routeTitle :: store -> Route store graph a -> Maybe Text
routeTitle store = \case
  Route_Index -> Just "Zettels"
  Route_Zettel zid ->
    let Zettel {..} = lookupStore zid store
     in Just zettelTitle
