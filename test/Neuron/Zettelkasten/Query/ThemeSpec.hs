{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.Query.ThemeSpec
  ( spec,
  )
where

import Data.Default (def)
import Neuron.Zettelkasten.Query.Theme
import Relude
import Test.Hspec
import Util

spec :: Spec
spec =
  describe "Link theme extraction from URI" $ do
    describe "Legacy link theme" $ do
      it "Parse linkTheme" $ do
        parseURIWith linkThemeFromURI "zquery://search?linkTheme=default"
          `shouldBe` Right (LinkView False)
        parseURIWith linkThemeFromURI "zquery://search?linkTheme=withDate"
          `shouldBe` Right (LinkView True)
      it "Parse 'simple' as default" $ do
        parseURIWith linkThemeFromURI "zcfquery://search?linkTheme=simple"
          `shouldBe` Right (LinkView False)
      it "Parse grouped query flag" $ do
        parseURIWith zettelsViewFromURI "zquery://search?tag=foo&grouped"
          `shouldBe` Right (ZettelsView def True)
        parseURIWith zettelsViewFromURI "zquery://search?tag=foo"
          `shouldBe` Right (ZettelsView def False)
    describe "Short links, theme" $ do
      it "accepts basic short link" $ do
        parseURIWith linkThemeFromURI "1234567"
          `shouldBe` Right (LinkView False)
