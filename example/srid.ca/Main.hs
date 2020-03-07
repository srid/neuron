{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Clay hiding (style, type_)
import qualified Clay as C
import Development.Shake
import Lucid
import qualified Neuron.Zettelkasten as Z
import qualified Neuron.Zettelkasten.Route as Z
import qualified Neuron.Zettelkasten.View as Z
import Path
import Relude
import qualified Rib
import Rib.Extra.CSS (googleFonts, stylesheet)

main :: IO ()
main = Z.run (thisDir </> [reldir|content|]) (thisDir </> [reldir|dest|]) generateSite
  where
    thisDir = [reldir|example/srid.ca|]

generateSite :: Action ()
generateSite = do
  let writeHtmlRoute r = Rib.writeRoute r . Lucid.renderText . renderPage r
  Z.generateSite writeHtmlRoute [[relfile|*.md|]]

renderPage :: Z.Route s g a -> (s, g) -> Html ()
renderPage route val = with html_ [lang_ "en"] $ do
  head_ $ do
    meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    -- TODO: open graph
    title_ $ toHtml $ maybe siteTitle (<> " - " <> siteTitle) $
      Z.routeTitle (fst val) route
    stylesheet "https://cdn.jsdelivr.net/npm/semantic-ui@3.4.2/dist/semantic.min.css"
    stylesheet "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.11.2/css/all.min.css"
    googleFonts $ [headerFont, bodyFont, monoFont]
    style_ [type_ "text/css"] $ C.render style
  body_ $ do
    div_ [class_ "ui text container", id_ "thesite"] $ do
      br_ mempty
      Z.renderRoute route val
  where
    siteTitle = "Example Zettelkasten"

headerFont :: Text
headerFont = "Oswald"

bodyFont :: Text
bodyFont = "Open Sans"

monoFont :: Text
monoFont = "Roboto Mono"

style :: Css
style = "div#thesite" ? do
  C.fontFamily [bodyFont] [C.serif]
  "p" ? do
    C.lineHeight $ pct 150
  "h1, h2, h3, h4, h5, h6" ? do
    C.fontFamily [headerFont] [C.sansSerif]
  Z.style
