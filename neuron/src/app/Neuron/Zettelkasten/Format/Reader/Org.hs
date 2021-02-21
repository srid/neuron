{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Format.Reader.Org
  ( parseOrg,
  )
where

import qualified Data.Map as Map
import Data.Text (toLower)
import qualified Data.YAML as Y
import qualified Data.YAML.Event as YE
import Neuron.Zettelkasten.Format.Reader.Type
import Relude
import Relude.Extra.Map (lookup)
import Text.Pandoc (def, runPure)
import Text.Pandoc.Definition (Pandoc)
import Text.Pandoc.Readers.Org (readOrg)
import Text.Pandoc.Util (getH1)

parseOrg :: ZettelParser
parseOrg _ s = do
  doc <- first show $ runPure $ readOrg def s
  meta <- extractMetadata doc
  pure (meta, doc)

-- | Extract metadata from the properties that are attached to the first headline
extractMetadata :: Pandoc -> Either ZettelParseError (Maybe (Y.Node Y.Pos))
extractMetadata doc
  | Just ((_, _, Map.fromList -> properties), _) <- getH1 doc = do
    let date = lookup "date" properties
        tags = words <$> lookup "tags" properties
        unlisted = parseUnlisted <$> lookup "unlisted" properties
    pure $
      Just $
        Y.Mapping
          noPos
          yTagMap
          $ Map.fromList $
            catMaybes
              [ unlisted <&> \x ->
                  (Y.Scalar noPos $ Y.SStr "unlisted", Y.Scalar noPos $ Y.SBool x),
                date <&> \x ->
                  (Y.Scalar noPos $ Y.SStr "date", Y.Scalar noPos $ Y.SStr x),
                tags <&> \xs ->
                  ( Y.Scalar noPos $ Y.SStr "tags",
                    Y.Sequence noPos yTagSeq $
                      xs <&> \x -> Y.Scalar noPos $ Y.SStr x
                  )
              ]
  | otherwise = pure Nothing
  where
    noPos = Y.Pos 0 0 0 0
    yTagMap = YE.mkTag "tag:yaml.org,2002:map"
    yTagSeq = YE.mkTag "tag:yaml.org,2002:seq"
    parseUnlisted :: Text -> Bool
    parseUnlisted a = toLower a == "true"
