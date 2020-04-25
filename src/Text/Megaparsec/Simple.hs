{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | A simple API for megaparsec
module Text.Megaparsec.Simple
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
