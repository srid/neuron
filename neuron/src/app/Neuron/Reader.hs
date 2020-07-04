{-# LANGUAGE LambdaCase #-}

module Neuron.Reader where

import Neuron.Reader.Markdown
import Neuron.Reader.Org
import Neuron.Reader.Type (ZettelFormat (..))
import Neuron.Zettelkasten.Zettel (ZettelReader)

readerForZettelFormat :: ZettelFormat -> ZettelReader
readerForZettelFormat = \case
  ZettelFormat_Markdown -> parseMarkdown
  ZettelFormat_Org -> parseOrg
