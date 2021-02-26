{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.ZettelSpec
  ( spec,
  )
where

import Data.Default
import Data.Tagged (Tagged (Tagged))
import Data.Time.Calendar (fromGregorian)
import Data.Time.DateMayTime (DateMayTime, mkDateMayTime)
import Data.Time.LocalTime
  ( LocalTime (LocalTime),
    TimeOfDay (TimeOfDay),
  )
import Neuron.Zettelkasten.ID (ZettelID (ZettelID))
import Neuron.Zettelkasten.Zettel
  ( MetadataOnly,
    Zettel,
    ZettelT (Zettel),
    sortZettelsReverseChronological,
  )
import Relude
import Test.Hspec

spec :: Spec
spec = do
  let noContent :: MetadataOnly = Tagged Nothing
  describe "sortZettelsReverseChronological" $ do
    let mkDay = fromGregorian 2020 3
        mkZettelDay n =
          Just $ mkDateMayTime $ Left (mkDay n)
        mkZettelLocalTime day hh mm =
          Just $ mkDateMayTime $ Right $ LocalTime (mkDay day) (TimeOfDay hh mm 0)

        mkZettel :: Text -> Maybe DateMayTime -> Zettel
        mkZettel s datetime =
          Zettel
            (ZettelID s)
            def
            s
            "<spec>.md"
            "Some title"
            False
            -- (Set.fromList [Tag "science", Tag "journal/class"])
            datetime
            False
            noContent
            mempty

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
