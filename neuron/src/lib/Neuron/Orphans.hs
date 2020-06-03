{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Neuron.Orphans where

import Data.Aeson
import Relude
import Text.URI (URI, mkURI, render)

instance ToJSON URI where
  toJSON = toJSON @Text . render

instance FromJSON URI where
  parseJSON = (either (fail . displayException) pure) . mkURI <=< parseJSON @Text
