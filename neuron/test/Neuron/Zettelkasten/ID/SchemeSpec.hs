{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.ID.SchemeSpec
  ( spec,
  )
where

import qualified Data.Set as Set
import Data.Time
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.ID.Scheme
import Relude
import Test.Hspec

spec :: Spec
spec = do
  describe "nextAvailableZettelID" $ do
    let zettels =
          Set.fromList $
            fmap
              (either (error . show) id . parseZettelID')
              [ "ribeye-steak",
                "2015403"
              ]
        day = fromGregorian 2020 4 16
        nextAvail scheme = do
          v <- genVal scheme
          pure $ nextAvailableZettelID zettels v scheme
    context "custom ID" $ do
      it "checks if already exists" $ do
        nextAvail (IDSchemeCustom "ribeye-steak")
          `shouldReturn` Left IDConflict_AlreadyExists
      it "succeeds" $ do
        nextAvail (IDSchemeCustom "sunny-side-eggs")
          `shouldReturn` Right (ZettelCustomID "sunny-side-eggs")
    context "date ID" $ do
      it "should return index 0" $ do
        let otherDay = fromGregorian 2020 5 16
        nextAvail (IDSchemeDate otherDay)
          `shouldReturn` Right (ZettelDateID otherDay 1)
      it "should return correct index" $
        nextAvail (IDSchemeDate day)
          `shouldReturn` Right (ZettelDateID day 4)
    context "hash ID" $ do
      it "should succeed" $
        nextAvail IDSchemeHash
          >>= (`shouldNotSatisfy` isLeft)
