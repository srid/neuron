{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.StoreSpec
  ( spec,
  )
where

import Control.Exception (evaluate)
import qualified Data.Map.Strict as Map
import Data.Time.Calendar
import qualified Neuron.Zettelkasten.ID as Z
import qualified Neuron.Zettelkasten.Store as Z
import Relude
import Test.Hspec

spec :: Spec
spec = do
  describe "Zettel Store" $ do
    let day = fromGregorian 2020 3 19
        emptyStore = Map.empty
        dateZid = Z.ZettelDateID day 1
        customZid = Z.ZettelCustomID "custom-id"
    context "nonexistent zettel ID lookup" $ do
      it "errors with text representation of date zettel ID" $ do
        evaluate (Z.lookupStore dateZid emptyStore) `shouldThrow` errorCall "No such zettel: 2011401"
      it "errors with custom zettel ID text" $ do
        evaluate (Z.lookupStore customZid emptyStore) `shouldThrow` errorCall "No such zettel: custom-id"
