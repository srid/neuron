{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.ZettelSpec
  ( spec,
  )
where

import Data.Aeson
import Data.TagTree
import Data.Time.Calendar
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Zettel
import Relude
import Rib.Parser.MMark (parsePure)
import Test.Hspec

spec :: Spec
spec = do
  describe "sortZettelsReverseChronological" $ do
    let mkDay = fromGregorian 2020 3
        dummyContent = either error id $ parsePure "<spec>" "Dummy"
        mkZettel day idx =
          Zettel (ZettelDateID (mkDay day) idx) "Some title" [Tag "science", Tag "journal/class"] Nothing dummyContent
    it "sorts correctly" $ do
      let zs = [mkZettel 3 2, mkZettel 5 1]
      sortZettelsReverseChronological zs
        `shouldBe` [mkZettel 5 1, mkZettel 3 2]
    it "sorts correctly even if same day" $ do
      let zs = [mkZettel 7 2, mkZettel 7 4]
      sortZettelsReverseChronological zs
        `shouldBe` [mkZettel 7 4, mkZettel 7 2]
  describe "Zettel JSON" $ do
    let day = fromGregorian 2020 3 19
        zid = ZettelCustomID "Foo-Bar"
        dummyContent = either error id $ parsePure "<spec>" "Dummy"
        zettel = Zettel zid "Some title" [Tag "science", Tag "journal/class"] (Just day) dummyContent
    it "Produces expected json" $ do
      -- "{\"id\":\"2011401\",\"title\":\"Some title\",\"tags\":[\"science\"]}"
      object (zettelJson zettel)
        `shouldBe` object
          [ "id" .= ("Foo-Bar" :: Text),
            "title" .= ("Some title" :: Text),
            "tags" .= (["science", "journal/class"] :: [Text]),
            "day" .= day
          ]
