{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Error
  ( NeuronError (..),
  )
where

import Neuron.Zettelkasten.ID (ZettelID, zettelIDSourceFileName, zettelIDText)
import Neuron.Zettelkasten.Link (InvalidNeuronLink (..))
import Neuron.Zettelkasten.Link.Theme (InvalidLinkTheme (..))
import Neuron.Zettelkasten.Query (InvalidQuery (..))
import Relude
import qualified Text.Show
import qualified Text.URI as URI

data NeuronError
  = -- A zettel file contains invalid link that neuron cannot parse
    NeuronError_BadLink ZettelID InvalidNeuronLink
  | -- A zettel file refers to another that does not exist
    NeuronError_BrokenZettelRef ZettelID ZettelID
  deriving (Eq)

instance Show NeuronError where
  show e =
    let fromZid = case e of
          NeuronError_BadLink zid _ -> zid
          NeuronError_BrokenZettelRef zid _ -> zid
        msg = case e of
          NeuronError_BadLink _ (InvalidNeuronLink uri le) ->
            "it contains a query URI (" <> URI.render uri <> ") " <> case le of
              Left (InvalidQuery_InvalidID s) ->
                "with invalid ID: " <> show s
              Left InvalidQuery_UnsupportedHost ->
                "with unsupported host"
              Left InvalidQuery_Unsupported ->
                "that is not supported"
              Right (InvalidLinkTheme theme) ->
                "with invalid link theme (" <> theme <> ")"
          NeuronError_BrokenZettelRef _fromZid toZid ->
            "it references a zettel <" <> zettelIDText toZid <> "> that does not exist"
     in toString $ unlines
          [ "",
            "  Zettel file \"" <> toText (zettelIDSourceFileName fromZid) <> "\" is malformed:",
            "    " <> msg
          ]
