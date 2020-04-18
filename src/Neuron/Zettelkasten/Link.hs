{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Special Zettel links in Markdown
module Neuron.Zettelkasten.Link where

import Neuron.Zettelkasten.Link.Action
import Neuron.Zettelkasten.Link.View
import Neuron.Zettelkasten.Store
import Relude
import qualified Text.MMark.Extension as Ext
import Text.MMark.Extension (Extension, Inline (..))

-- | MMark extension to transform @z:/@ links in Markdown
linkActionExt :: ZettelStore -> Extension
linkActionExt store =
  Ext.inlineRender $ \f -> \case
    inline@(Link inner uri _title) ->
      let mlink = MarkdownLink (Ext.asPlainText inner) uri
       in case linkActionFromLink mlink of
            Just lact ->
              linkActionRender store lact
            Nothing ->
              f inline
    inline ->
      f inline
