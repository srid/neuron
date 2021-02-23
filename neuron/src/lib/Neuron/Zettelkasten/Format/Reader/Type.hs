{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Format.Reader.Type where

import Data.Tagged (Tagged)
import qualified Data.YAML as Y
import Relude hiding (readEither, show)
import Text.Pandoc.Definition (Pandoc)

type ZettelParser =
  -- | Source filepath (used only in error messages)
  FilePath ->
  -- | Text to parse
  Text ->
  -- | Parse result: a tuple of YAML metadata and Pandoc AST.
  Either
    ZettelParseError
    (Maybe (Y.Node Y.Pos), Pandoc)

type ZettelParseError = Tagged "ZettelParserError" Text
