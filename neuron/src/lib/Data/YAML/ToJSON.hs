{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.YAML.ToJSON where

import Data.Aeson
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encoding as AesonEncoding
import qualified Data.YAML as Y
import Relude

instance Aeson.ToJSONKey (Y.Node Y.Pos) where
  toJSONKey = ToJSONKeyText f (AesonEncoding.text . f)
    where 
      f = \case
        Y.Scalar _ x -> scalarToText x
        Y.Mapping _ _ x -> show x
        Y.Sequence _ _ x -> show x
        Y.Anchor _ _ x -> f x
      scalarToText = \case  
        Y.SNull -> "null"
        Y.SBool x -> show x
        Y.SFloat x -> show x
        Y.SInt x -> show x
        Y.SStr x -> x
        Y.SUnknown _tag x -> show x

instance Aeson.ToJSON (Y.Node Y.Pos) where
  toJSON = \case
    Y.Scalar _ x -> toJSON x
    Y.Mapping _ _ x -> toJSON x
    Y.Sequence _ _ x -> toJSON x
    Y.Anchor _ _ _ -> toJSON ("unsupported" :: Text)

instance ToJSON Y.Scalar where
  toJSON = \case
    Y.SNull -> Aeson.Null
    Y.SBool x -> toJSON x
    Y.SFloat x -> toJSON x
    Y.SInt x -> toJSON x
    Y.SStr x -> toJSON x
    Y.SUnknown _tag x -> toJSON x

