{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Error
  ( NeuronError (..),
    neuronErrorReason,
  )
where

import Neuron.Zettelkasten.ID (ZettelID, zettelIDSourceFileName, zettelIDText)
import Neuron.Zettelkasten.Query.Error
import Relude
import qualified Text.Show
import qualified Text.URI as URI

data NeuronError
  = -- A zettel file contains invalid link that neuron cannot parse
    NeuronError_BadQuery ZettelID QueryError
  deriving (Eq)

-- | The reason this particular zettel failed to process fully.
neuronErrorReason :: NeuronError -> Text
neuronErrorReason (NeuronError_BadQuery _fromZid e) =
  case e of
    Left qe ->
      "The query URI (" <> URI.render (queryParseErrorUri qe) <> ") " <> case qe of
        QueryParseError_UnsupportedHost _uri ->
          "has an unsupported host"
        QueryParseError_InvalidID _uri e'' ->
          "has an invalidID: " <> show e''
    Right (QueryResultError_NoSuchZettel zid) ->
      "Zettel with ID \""
        <> zettelIDText zid
        <> "\" does not exist"

instance Show NeuronError where
  show err@(NeuronError_BadQuery fromZid _e) =
    let msg = neuronErrorReason err
     in toString $
          unlines
            [ "",
              "  Zettel file \"" <> toText (zettelIDSourceFileName fromZid) <> "\" is malformed:",
              "    " <> msg
            ]
