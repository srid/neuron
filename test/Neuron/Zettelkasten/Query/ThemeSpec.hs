{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Query.ThemeSpec
  ( spec,
  )
where

import Neuron.Zettelkasten.Query.Theme
import Relude
import Test.Hspec
import Util

spec :: Spec
spec =
  describe "URI parsing" $ do
    describe "Legacy" $ do
      it "Parse linkTheme" $ do
        parseURIWith zettelsViewFromURI "zquery://search?linkTheme=default"
          `shouldBe` Right (Just $ ZettelsView (LinkView False) False)
        parseURIWith zettelsViewFromURI "zquery://search?linkTheme=withDate"
          `shouldBe` Right (Just $ ZettelsView (LinkView True) False)
      it "Parse 'simple' as default" $ do
        parseURIWith zettelsViewFromURI "zcfquery://search?linkTheme=simple"
          `shouldBe` Right (Just $ ZettelsView (LinkView False) False)
      it "Parse grouped query flag" $ do
        parseURIWith zettelsViewFromURI "zquery://search?tag=foo&grouped"
          `shouldBe` Right (Just $ ZettelsView (LinkView False) True)
        parseURIWith zettelsViewFromURI "zquery://search?tag=foo"
          `shouldBe` Right Nothing
    describe "Short links" $ do
      it "accepts basic short link" $ do
        parseURIWith zettelsViewFromURI "1234567"
          `shouldBe` Right Nothing
