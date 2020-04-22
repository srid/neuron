{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.ConfigSpec
  ( spec,
  )
where

import qualified Neuron.Config as Z
import Neuron.Parser
import qualified Neuron.Zettelkasten.ID as Z
import Relude
import Test.Hspec

spec :: Spec
spec = do
  describe "Alias parsing" $ do
    itParsesAlias "with z-index" "index:z-index"
    itParsesAlias "with normal id" "foo:2011501"
    itParsesAlias "longish alias" "tis-a-furphy:2011501"

itParsesAlias :: String -> Text -> SpecWith ()
itParsesAlias name s =
  it name $ do
    fmap renderAlias (parse Z.aliasParser "<hspec>" s) `shouldBe` Right s
  where
    renderAlias Z.Alias {..} =
      Z.zettelIDText aliasZettel <> ":" <> Z.zettelIDText targetZettel
