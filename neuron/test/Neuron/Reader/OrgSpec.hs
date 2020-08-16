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
    itParsesDay "with day" "2020-08-16"
    itParsesDate "with localtime" "2020-08-16T09:42"
    itParsesInvalid "with invalid" "2020-08-16 09:42"

itParsesDay :: String -> Text -> SpecWith ()
itParsesDay name s =
  it name $ do
    parseDate s `shouldBe` Right (Left (ModifiedJulianDay 59077))

itParsesDate :: String -> Text -> SpecWith ()
itParsesDate name s =
  it name $ do
    parseDate s `shouldBe` Right (Right (LocalTime (ModifiedJulianDay 59077) (TimeOfDay 9 42 0)))

itParsesInvalid :: String -> Text -> SpecWith ()
itParsesInvalid name s =
  it name $ do
    parseDate s `shouldBe` Left (Tagged $ "Invalid date format: " <> s)
