{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.ZettelSpec
  ( spec,
  )
where

import Data.Aeson
import Data.Time.Calendar
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Tag
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
        zettel = Zettel zid "Some title" [Tag "science", Tag "journal/class"] dummyContent
    it "Produces expected json" $ do
      -- "{\"id\":\"2011401\",\"title\":\"Some title\",\"tags\":[\"science\"]}"
      object (zettelJson zettel)
        `shouldBe` object
          [ "id" .= ("2011401" :: Text),
            "title" .= ("Some title" :: Text),
            "tags" .= (["science", "journal/class"] :: [Text])
          ]
