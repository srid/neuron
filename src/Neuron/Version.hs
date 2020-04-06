{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Neuron version
module Neuron.Version where

import qualified Data.Text as T
import Data.Version (parseVersion, showVersion)
import qualified Neuron.Version.RepoVersion as RepoVersion
import Paths_neuron (version)
import Relude
import Text.ParserCombinators.ReadP (readP_to_S)

neuronVersion :: Text
neuronVersion = toText $ showVersion version

neuronVersionFull :: Text
neuronVersionFull =
  T.concat [neuronVersion, " (", RepoVersion.version, ")"]

-- | Check if neuronVersion is older than the given version
olderThan :: Text -> Bool
olderThan s =
  case reverse (readP_to_S parseVersion (toString s)) of
    (v2, _) : _ -> version < v2
    x -> error $ "Version parse error: " <> show x
