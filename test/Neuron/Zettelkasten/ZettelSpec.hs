{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.ZettelSpec
  ( spec,
  )
where

import Data.Aeson
import Data.Time.Calendar
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Zettel
import Relude
import Rib.Parser.MMark (parsePure)
import Test.Hspec

spec :: Spec
spec = do
  describe "Zettel JSON" $ do
    let day = fromGregorian 2020 3 19
        zid = ZettelDateID day 1
        dummyContent = either error id $ parsePure "<spec>" "Dummy"
        zettel = Zettel zid "Some title" ["science"] dummyContent
    it "Produces expected json" $ do
      encode zettel `shouldBe` "{\"id\":\"2011401\",\"title\":\"Some title\",\"tags\":[\"science\"]}"
