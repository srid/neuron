module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Web.HTML (window)
import Web.HTML.Window (document)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.DOM.NonElementParentNode (getElementById)
-- import Debug.Trace (traceM)

main :: Effect Unit
main = do
  doc <- map toNonElementParentNode $ window >>= document
  mayElem <- getElementById "thesite" doc
  -- traceM mayElem
  log "🍝"
