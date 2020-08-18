{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Reader.OrgSpec
  ( spec,
  )
where

import Data.Tagged (Tagged (Tagged))
import Data.Time (LocalTime (LocalTime))
import Data.Time.Calendar
import Data.Time.LocalTime (TimeOfDay (TimeOfDay))
import Neuron.Reader.Org
import Relude
import Test.Hspec

spec :: Spec
spec = do
  describe "date-tag parsing" $ do
    it "with day" $ do
      parseDate "2020-08-16" `shouldBe` Right (Left (ModifiedJulianDay 59077))
    it "with localtime" $ do
      parseDate "2020-08-16T09:42" `shouldBe` Right (Right (LocalTime (ModifiedJulianDay 59077) (TimeOfDay 9 42 0)))
    it "with invalid" $ do
      parseDate "2020-08-16 09:42" `shouldBe` Left (Tagged $ "Invalid date format: " <> "2020-08-16 09:42")
