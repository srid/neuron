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
    NeuronError_BadQuery ZettelID QueryError
  deriving (Eq)

instance Show NeuronError where
  show (NeuronError_BadQuery fromZid e) =
    let msg = case e of
          Left qe ->
            "it contains a query URI (" <> URI.render (queryParseErrorUri qe) <> ") " <> case qe of
              QueryParseError_UnsupportedHost _uri ->
                "with unsupported host"
              QueryParseError_InvalidID _uri e'' ->
                "with invalidID: " <> show e''
              QueryParseError_BadView _uri (InvalidLinkView view) ->
                "with invalid link view (" <> view <> ")"
          Right (QueryResultError_NoSuchZettel zid) ->
            "Zettel "
              <> zettelIDText zid
              <> " does not exist"
     in toString $
          unlines
            [ "",
              "  Zettel file \"" <> toText (zettelIDSourceFileName fromZid) <> "\" is malformed:",
              "    " <> msg
            ]
