{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Parser
  ( Parser,
    parse,
  )
where

import Relude
import qualified Text.Megaparsec as M

type Parser a = M.Parsec Void Text a

parse :: Parser a -> String -> Text -> Either Text a
parse p fn s =
  first (toText . M.errorBundlePretty) $
    M.parse (p <* M.eof) fn s
