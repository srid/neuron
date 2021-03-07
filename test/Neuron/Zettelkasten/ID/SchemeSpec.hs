{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.ID.SchemeSpec
  ( spec,
  )
where

import qualified Data.Set as Set
import Neuron.Zettelkasten.ID (ZettelID (ZettelID), parseZettelID)
import Neuron.Zettelkasten.ID.Scheme
  ( IDConflict (IDConflict_AlreadyExists),
    IDScheme (IDSchemeCustom, IDSchemeHash),
    genVal,
    nextAvailableZettelID,
  )
import Relude
import Test.Hspec

spec :: Spec
spec = do
  describe "nextAvailableZettelID" $ do
    let zettels =
          Set.fromList $
            fmap
              (either (error . show) id . parseZettelID)
              [ "ribeye-steak",
                "2015403"
              ]
        nextAvail scheme = do
          v <- genVal scheme
          pure $ nextAvailableZettelID zettels v scheme
    context "custom ID" $ do
      it "checks if already exists" $ do
        nextAvail (IDSchemeCustom "ribeye-steak")
          `shouldReturn` Left IDConflict_AlreadyExists
      it "succeeds" $ do
        nextAvail (IDSchemeCustom "sunny-side-eggs")
          `shouldReturn` Right (ZettelID "sunny-side-eggs")
    context "hash ID" $ do
      it "should succeed" $
        nextAvail IDSchemeHash
          >>= (`shouldNotSatisfy` isLeft)
