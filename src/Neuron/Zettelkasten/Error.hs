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
import Relude
import qualified Text.Show
import qualified Text.URI as URI

data NeuronError
  = -- A zettel file contains invalid link that neuron cannot parse
    NeuronError_BadQuery ZettelID QueryError
  deriving (Eq)

instance Show NeuronError where
  show e =
    let fromZid = case e of
          NeuronError_BadQuery zid _ -> zid
        msg = case e of
          NeuronError_BadQuery _fromZid qe ->
            "it contains a query URI (" <> URI.render (queryErrorUri qe) <> ") " <> case qe of
              QueryError_ZettelNotFound _fromZid toZid ->
                "which references a zettel <" <> zettelIDText toZid <> "> that does not exist"
              QueryError_InvalidQuery _ e' -> case e' of
                InvalidQuery_UnsupportedHost ->
                  "with unsupported host"
                InvalidQuery_Unsupported ->
                  "that is not supported"
                InvalidQuery_InvalidID e'' ->
                  "with invalidID: " <> show e''
              QueryError_InvalidQueryView _ e' -> case e' of
                InvalidLinkView view ->
                  "with invalid link view (" <> view <> ")"
     in toString $
          unlines
            [ "",
              "  Zettel file \"" <> toText (zettelIDSourceFileName fromZid) <> "\" is malformed:",
              "    " <> msg
            ]
