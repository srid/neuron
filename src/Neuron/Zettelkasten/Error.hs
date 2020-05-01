{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Error
  ( NeuronError (..),
  )
where

import Neuron.Zettelkasten.ID (ZettelID, zettelIDSourceFileName, zettelIDText)
import Neuron.Zettelkasten.Query.Error
import Neuron.Zettelkasten.Query.Theme (InvalidLinkView (..))
import Relude
import qualified Text.Show
import qualified Text.URI as URI

data NeuronError
  = -- A zettel file contains invalid link that neuron cannot parse
    NeuronError_BadQuery ZettelID QueryParseError
  | -- Running the query did not produce expected result
    NeuronError_QueryFailed QueryResultError
  deriving (Eq)

instance Show NeuronError where
  show e =
    let fromZid = case e of
          NeuronError_BadQuery zid _ -> zid
          NeuronError_QueryFailed (QueryResultError_NoSuchZettel zid) -> zid
        msg = case e of
          NeuronError_BadQuery _fromZid qe ->
            "it contains a query URI (" <> URI.render (queryParseErrorUri qe) <> ") " <> case qe of
              QueryParseError_UnsupportedHost _uri ->
                "with unsupported host"
              QueryParseError_Unsupported _uri ->
                "that is not supported"
              QueryParseError_InvalidID _uri e'' ->
                "with invalidID: " <> show e''
              QueryParseError_BadView _uri (InvalidLinkView view) ->
                "with invalid link view (" <> view <> ")"
          NeuronError_QueryFailed (QueryResultError_NoSuchZettel zid) ->
            "Zettel "
              <> zettelIDText zid
              <> " does not exist"
     in toString $
          unlines
            [ "",
              "  Zettel file \"" <> toText (zettelIDSourceFileName fromZid) <> "\" is malformed:",
              "    " <> msg
            ]
