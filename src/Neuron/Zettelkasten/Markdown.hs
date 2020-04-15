{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Neuron.Zettelkasten.Markdown
  ( neuronMMarkExts,
  )
where

import Neuron.Config (Config (..))
import Neuron.Zettelkasten.Markdown.Extension (setTableClass)
import Relude
import qualified Text.MMark as MMark
import qualified Text.MMark.Extension.Common as Ext

neuronMMarkExts :: Config -> [MMark.Extension]
neuronMMarkExts Config {..} =
  defaultExts
    <> bool [] [Ext.mathJax (Just '$')] mathJaxSupport

defaultExts :: [MMark.Extension]
defaultExts =
  [ Ext.fontAwesome,
    Ext.footnotes,
    Ext.kbd,
    Ext.linkTarget,
    Ext.punctuationPrettifier,
    Ext.skylighting,
    setTableClass "ui celled table"
  ]
