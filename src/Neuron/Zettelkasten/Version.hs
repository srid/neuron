{-# LANGUAGE TemplateHaskell #-}
-- | Zettelkasten version

module Neuron.Zettelkasten.Version where

import Development.GitRev (gitDescribe)

version :: String
version = $(gitDescribe)
