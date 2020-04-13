{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Neuron.Zettelkasten.Markdown.Extension
  ( setTableClass,
  )
where

import qualified Data.List.NonEmpty as NE
import Lucid
import Relude
import Text.MMark.Extension (Block (Table), Extension)
import qualified Text.MMark.Extension as Ext

newline :: Html ()
newline = "\n"

setTableClass :: Text -> Extension
setTableClass tableClass = Ext.blockRender $ \old block ->
  case block of
    -- https://github.com/mmark-md/mmark/blob/8f5534d8068c2b7a139b893639ee5920bcaedd84/Text/MMark/Render.hs#L111-L136
    (Table calign (hs :| rows)) -> do
      table_ [class_ tableClass] $ do
        newline
        thead_ $ do
          newline
          tr_
            $ forM_ (NE.zip calign hs)
            $ \(a, h) ->
              th_ (alignStyle a) (snd h)
          newline
        newline
        tbody_ $ do
          newline
          forM_ rows $ \row -> do
            tr_
              $ forM_ (NE.zip calign row)
              $ \(a, h) ->
                td_ (alignStyle a) (snd h)
            newline
        newline
      newline
    other -> old other
  where
    alignStyle Ext.CellAlignDefault = []
    alignStyle Ext.CellAlignLeft = [style_ "text-align:left"]
    alignStyle Ext.CellAlignRight = [style_ "text-align:right"]
    alignStyle Ext.CellAlignCenter = [style_ "text-align:center"]
