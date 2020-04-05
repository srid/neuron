{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.IDSpec
  ( spec,
  )
where

import Data.Time.Calendar
import qualified Neuron.Zettelkasten.ID as Z
import Path
import Relude
import Test.Hspec

spec :: Spec
spec = do
  describe "Zettel ID" $ do
    context "parsing" $ do
      let zid = Z.ZettelID "2011401"
      it "parses a zettel ID" $ do
        Z.parseZettelID "2011401" `shouldBe` zid
      it "parses a zettel ID from zettel filename" $ do
        Z.mkZettelID [relfile|2011401.md|] `shouldBe` zid
        Z.zettelIDSourceFileName zid `shouldBe` "2011401.md"
      it "returns the correct day" $ do
        Z.zettelIDDate zid `shouldBe` fromGregorian 2020 3 19
