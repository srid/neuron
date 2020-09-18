{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.ZettelSpec
  ( spec,
  )
where

import Data.TagTree
import Data.Time.Calendar
import Data.Time.DateMayTime
import Data.Time.LocalTime
import Neuron.Reader.Markdown
import Neuron.Reader.Type
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Zettel
import Neuron.Zettelkasten.Zettel.Meta
import Relude
import Test.Hspec

spec :: Spec
spec = do
  let noQueries = mempty -- TODO: test queries
      noError = Right mempty
      noContent = MetadataOnly ()
  describe "sortZettelsReverseChronological" $ do
    let mkDay = fromGregorian 2020 3
        mkZettelDay n =
          Just $ mkDateMayTime $ Left (mkDay n)
        mkZettelLocalTime day hh mm =
          Just $ mkDateMayTime $ Right $ LocalTime (mkDay day) (TimeOfDay hh mm 0)

        (_ :: Maybe Meta, _dummyContent) = either (error . show) id $ parseMarkdown "<spec>" "Dummy"

        mkZettel zid datetime =
          Zettel
            (unsafeMkZettelID zid)
            ZettelFormat_Markdown
            "<spec>.md"
            "Some title"
            False
            [Tag "science", Tag "journal/class"]
            datetime
            False
            noQueries
            noError
            noContent

    it "sorts correctly with day" $ do
      let zs =
            [ mkZettel "a" (mkZettelDay 3),
              mkZettel "b" (mkZettelDay 5)
            ]
      sortZettelsReverseChronological zs
        `shouldBe` reverse zs

    it "sorts correctly with localtime" $ do
      let zs =
            [ mkZettel "a" (mkZettelLocalTime 3 9 59),
              mkZettel "b" (mkZettelLocalTime 3 10 0)
            ]
      sortZettelsReverseChronological zs
        `shouldBe` reverse zs

    it "sorts correctly with mixed dates" $ do
      let zs =
            [ mkZettel "c" (mkZettelLocalTime 7 0 0),
              mkZettel "a" (mkZettelDay 5),
              mkZettel "b" (mkZettelLocalTime 3 0 0)
            ]
      sortZettelsReverseChronological zs
        `shouldBe` zs
