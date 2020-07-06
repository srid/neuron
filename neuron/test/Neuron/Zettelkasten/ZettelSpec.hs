{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Neuron.Zettelkasten.ZettelSpec
  ( spec,
  )
where

import Data.TagTree
import Data.Time.Calendar
import Neuron.Reader.Markdown
import Neuron.Reader.Type
import Neuron.Zettelkasten.ID
import Neuron.Zettelkasten.Zettel
import Neuron.Zettelkasten.Zettel.Meta (Meta)
import Relude
import Test.Hspec

spec :: Spec
spec = do
  let noQueries = mempty -- TODO: test queries
      noError = Right mempty
      noContent = MetadataOnly ()
  describe "sortZettelsReverseChronological" $ do
    let mkDay = fromGregorian 2020 3
        (_ :: Maybe Meta, _dummyContent) = either (error . show) id $ parseMarkdown "<spec>" "Dummy"
        mkZettel day idx =
          Zettel
            (ZettelDateID (mkDay day) idx)
            ZettelFormat_Markdown
            "<spec>.md"
            "Some title"
            False
            [Tag "science", Tag "journal/class"]
            (Just $ mkDay day)
            noQueries
            noError
            noContent
    it "sorts correctly" $ do
      let zs = [mkZettel 3 2, mkZettel 5 1]
      sortZettelsReverseChronological zs
        `shouldBe` [mkZettel 5 1, mkZettel 3 2]
