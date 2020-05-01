{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Error
  ( NeuronError (..),
  )
where

import Neuron.Zettelkasten.ID (ZettelID, zettelIDSourceFileName, zettelIDText)
import Neuron.Zettelkasten.Query.Error (InvalidQuery (..), QueryError (..), queryErrorUri)
import Neuron.Zettelkasten.Query.Theme (InvalidLinkView (..))
import Neuron.Zettelkasten.Query.View (QueryNoData (..))
import Relude
import qualified Text.Show
import qualified Text.URI as URI

data NeuronError
  = -- A zettel file contains invalid link that neuron cannot parse
    NeuronError_BadQuery ZettelID QueryError
  | -- Running the query did not produce expected result
    NeuronError_QueryFailed QueryNoData
  deriving (Eq)

instance Show NeuronError where
  show e =
    let fromZid = case e of
          NeuronError_BadQuery zid _ -> zid
          NeuronError_QueryFailed (QueryNoData_NoSuchZettel zid) -> zid
        msg = case e of
          NeuronError_BadQuery _fromZid qe ->
            "it contains a query URI (" <> URI.render (queryErrorUri qe) <> ") " <> case qe of
              QueryError_InvalidQuery _ e' -> case e' of
                InvalidQuery_UnsupportedHost ->
                  "with unsupported host"
                InvalidQuery_Unsupported ->
                  "that is not supported"
                InvalidQuery_InvalidID e'' ->
                  "with invalidID: " <> show e''
                InvalidQuery_BadView (InvalidLinkView view) ->
                  "with invalid link view (" <> view <> ")"
          NeuronError_QueryFailed (QueryNoData_NoSuchZettel zid) ->
            "Zettel "
              <> zettelIDText zid
              <> " does not exist"
     in toString $
          unlines
            [ "",
              "  Zettel file \"" <> toText (zettelIDSourceFileName fromZid) <> "\" is malformed:",
              "    " <> msg
            ]
