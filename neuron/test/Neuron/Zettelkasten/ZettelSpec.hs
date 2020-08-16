{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.ZettelSpec
  ( spec,
  )
where

import Data.TagTree
import Data.Time (LocalTime (LocalTime))
import Data.Time.Calendar
import Data.Time.LocalTime (TimeOfDay (TimeOfDay))
import Neuron.Reader.Markdown
import Neuron.Reader.Type
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Zettel
import Neuron.Zettelkasten.Zettel.Meta (Meta)
import Relude
import Test.Hspec

spec :: Spec
spec = do
  let noQueries = mempty -- TODO: test queries
      noError = Right mempty
      noContent = MetadataOnly ()
  describe "sortZettelsReverseChronological" $ do
    let mkDay = fromGregorian 2020 3
        mkZettelDay = Just . Left . mkDay
        mkZettelLocalTime day hh mm = (Just (Right $ (LocalTime (mkDay day) (TimeOfDay hh mm 0))))

        (_ :: Maybe Meta, _dummyContent) = either (error . show) id $ parseMarkdown "<spec>" "Dummy"

        mkZettel day date idx =
          Zettel
            (ZettelDateID (mkDay day) idx)
            ZettelFormat_Markdown
            "<spec>.md"
            "Some title"
            False
            [Tag "science", Tag "journal/class"]
            date
            False
            noQueries
            noError
            noContent

    it "sorts correctly with day" $ do
      let zs = [mkZettel 3 (mkZettelDay 3) 2, mkZettel 5 (mkZettelDay 5) 1]
      sortZettelsReverseChronological zs
        `shouldBe` reverse zs

    it "sorts correctly with localtime" $ do
      let zs = [mkZettel 3 (mkZettelLocalTime 3 9 59) 2, mkZettel 3 (mkZettelLocalTime 3 10 0) 1]
      sortZettelsReverseChronological zs
        `shouldBe` reverse zs

    it "sorts correctly with mixed dates" $ do
      let zs = [mkZettel 3 (mkZettelDay 3) 2, mkZettel 3 (mkZettelLocalTime 3 0 0) 1]
      sortZettelsReverseChronological zs
        `shouldBe` reverse zs
