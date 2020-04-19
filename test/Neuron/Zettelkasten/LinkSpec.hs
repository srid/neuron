{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.LinkSpec
  ( spec,
  )
where

import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Link
import Neuron.Zettelkasten.Link.Theme (LinkTheme (..), ZettelsView (..))
import Neuron.Zettelkasten.Markdown (MarkdownLink (..))
import Neuron.Zettelkasten.Query
import Neuron.Zettelkasten.Tag
import Relude
import Test.Hspec
import Text.URI

spec :: Spec
spec =
  describe "NeuronLink" $ do
    let zid = parseZettelID "1234567"
        zettelsView = ZettelsView LinkTheme_Default False
    it "alias link" $
      mkMarkdownLink "1234567" "1234567"
        `shouldParseAs` Just (NeuronLink (Query_ZettelByID zid, Folgezettel, LinkTheme_Default))
    it "not an alias link (different link text)" $
      mkMarkdownLink "foo" "1234567"
        `shouldParseAs` Nothing
    it "z: link" $
      mkMarkdownLink "1234567" "z:"
        `shouldParseAs` Just (NeuronLink (Query_ZettelByID zid, Folgezettel, LinkTheme_Default))
    it "z: link, with annotations" $
      mkMarkdownLink "1234567" "z://foo-bar"
        `shouldParseAs` Just (NeuronLink (Query_ZettelByID zid, Folgezettel, LinkTheme_Default))
    it "zcf: link" $
      mkMarkdownLink "1234567" "zcf:"
        `shouldParseAs` Just (NeuronLink (Query_ZettelByID zid, OrdinaryConnection, LinkTheme_Default))
    it "zcf: link, with annotations" $
      mkMarkdownLink "1234567" "zcf://foo-bar"
        `shouldParseAs` Just (NeuronLink (Query_ZettelByID zid, OrdinaryConnection, LinkTheme_Default))
    it "zquery: link" $
      mkMarkdownLink "." "zquery://search?tag=science"
        `shouldParseAs` Just (NeuronLink (Query_ZettelsByTag [mkTagPattern "science"], Folgezettel, zettelsView))
    it "zcfquery: link" $
      mkMarkdownLink "." "zcfquery://search?tag=science"
        `shouldParseAs` Just (NeuronLink (Query_ZettelsByTag [mkTagPattern "science"], OrdinaryConnection, zettelsView))
    it "normal link" $ do
      mkMarkdownLink "foo bar" "https://www.google.com"
        `shouldParseAs` Nothing
      mkMarkdownLink "https://www.google.com" "https://www.google.com"
        `shouldParseAs` Nothing
  where
    shouldParseAs ml nl =
      neuronLinkFromMarkdownLink ml `shouldBe` Right nl

mkMarkdownLink :: Text -> Text -> MarkdownLink
mkMarkdownLink s l =
  MarkdownLink s $ either (error . toText . displayException) id $ mkURI l
