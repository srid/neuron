{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Options.Applicative.Extra where

import Options.Applicative
import Relude
import System.FilePath (addTrailingPathSeparator)
import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as M

hostPortOption :: Parser (Maybe (Text, Int))
hostPortOption =
  optional
    ( option
        (megaparsecReader hostPortParser)
        ( long "serve"
            <> short 's'
            <> metavar "[HOST]:PORT"
            <> help "Run a HTTP server on the generated directory"
        )
    )
    <|> fmap
      (bool Nothing $ Just (defaultHost, 8080))
      ( switch (short 'S' <> help ("Like `-s " <> toString defaultHost <> ":8080`"))
      )
  where
    hostPortParser :: M.Parsec Void Text (Text, Int)
    hostPortParser = do
      host <-
        optional $
          M.string "localhost"
            <|> M.try parseIP
      void $ M.char ':'
      port <- parseNumRange 1 65535
      pure (fromMaybe defaultHost host, port)
      where
        readNum = maybe (fail "Not a number") pure . readMaybe
        parseIP :: M.Parsec Void Text Text
        parseIP = do
          a <- parseNumRange 0 255 <* M.char '.'
          b <- parseNumRange 0 255 <* M.char '.'
          c <- parseNumRange 0 255 <* M.char '.'
          d <- parseNumRange 0 255
          pure $ toText $ intercalate "." $ show <$> [a, b, c, d]
        parseNumRange :: Int -> Int -> M.Parsec Void Text Int
        parseNumRange a b = do
          n <- readNum =<< M.some M.digitChar
          if a <= n && n <= b
            then pure n
            else fail $ "Number not in range: " <> show a <> "-" <> show b

    defaultHost :: Text
    defaultHost = "127.0.0.1"

megaparsecReader :: M.Parsec Void Text a -> ReadM a
megaparsecReader p =
  eitherReader (first M.errorBundlePretty . M.parse p "<optparse-input>" . toText)

-- | Like `str` but adds a trailing slash if there isn't one.
directoryReader :: ReadM FilePath
directoryReader = fmap addTrailingPathSeparator str
