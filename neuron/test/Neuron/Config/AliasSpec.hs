{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Config.AliasSpec
  ( spec,
  )
where

import Neuron.Config.Alias (Alias (..), aliasParser)
import Neuron.Zettelkasten.ID (ZettelID (zettelIDSlug))
import Relude
import Test.Hspec
import Text.Megaparsec.Simple (parse)

spec :: Spec
spec = do
  describe "Alias parsing" $ do
    itParsesAlias "with z-index" "index:z-index"
    itParsesAlias "with normal id" "foo:2011501"
    itParsesAlias "longish alias" "tis-a-furphy:2011501"

itParsesAlias :: String -> Text -> SpecWith ()
itParsesAlias name s =
  it name $ do
    fmap renderAlias (parse aliasParser "<hspec>" s) `shouldBe` Right s
  where
    renderAlias Alias {..} =
      zettelIDSlug aliasZettel <> ":" <> zettelIDSlug targetZettel
