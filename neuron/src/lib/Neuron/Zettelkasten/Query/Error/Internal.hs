{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Neuron.Zettelkasten.Query.Error.Internal where

import Data.Aeson
import Relude
import Text.URI (URI, render)

instance ToJSON URI where
  toJSON = toJSON @Text . render
