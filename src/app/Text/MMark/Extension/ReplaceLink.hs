{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Text.MMark.Extension.ReplaceLink
  ( replaceLink,
  )
where

import Lucid
import Relude
import Relude.Extra.Map
import Text.MMark.Extension (Extension)
import qualified Text.MMark.Extension as Ext
import Text.MMark.MarkdownLink

-- | MMark extension to replace links with some HTML.
replaceLink :: Map MarkdownLink (Html ()) -> Extension
replaceLink linkMap =
  Ext.inlineRender $ \f -> \case
    inline@(Ext.Link inner uri _title) ->
      MarkdownLink (Ext.asPlainText inner) uri
        & flip lookup linkMap
        & fromMaybe (f inline)
    inline ->
      f inline
