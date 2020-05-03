{-# LANGUAGE NoImplicitPrelude #-}

module Text.MMark.Extension.SetTableClass
  ( setTableClass,
  )
where

import Lucid
import Relude
import Text.MMark.Extension (Block (Table), Extension)
import qualified Text.MMark.Extension as Ext

setTableClass :: Text -> Extension
setTableClass tableClass = Ext.blockRender $ \old block ->
  case block of
    table@(Table _ _) -> with (old table) [class_ tableClass]
    other -> old other
