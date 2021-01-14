{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Query.Parser
  ( parseQueryLink,
  )
where

import Data.Default (Default (def))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Neuron.Zettelkasten.Connection (Connection (..))
import Neuron.Zettelkasten.ID
import Relude hiding (traceShowId)

-- | Parse a query if any from a Markdown link
-- TODO: queryConn should be read from link attribute!
parseQueryLink :: [(Text, Text)] -> Text -> Maybe (ZettelID, Connection)
parseQueryLink attrs url = do
  let conn = case Map.lookup "title" (Map.fromList attrs) of
        Just s -> if s == show Folgezettel then Folgezettel else def
        _ -> def
  path <- determineLinkType url
  -- Allow raw filename (ending with ".md").
  zid <- getZettelID (toString path)
  pure (zid, conn)
  where
    -- NOTE: This treats "foo.html" as zettel ref (why shouldn't it?), but not
    -- "./foo.html"
    determineLinkType :: Text -> Maybe Text
    determineLinkType s = do
      guard $ not $ "/" `T.isInfixOf` s || ":" `T.isInfixOf` s
      pure s
