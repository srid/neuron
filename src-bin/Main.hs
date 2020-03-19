{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Clay hiding (s, style, type_)
import qualified Clay as C
import Development.Shake
import Lucid
-- TODO: Don't expose every module
import qualified Neuron.Zettelkasten as Z
import qualified Neuron.Zettelkasten.Route as Z
import qualified Neuron.Zettelkasten.View as Z
import Path
import Relude
import qualified Rib
import Rib.Extra.CSS (googleFonts, stylesheet)

main :: IO ()
main = Z.run generateSite

generateSite :: Action ()
generateSite = do
  Rib.buildStaticFiles [[relfile|static/**|]]
  let site :: Z.Site =
        Z.Site
          { Z.siteTitle = "My Zettelkasten",
            Z.siteAuthor = Nothing,
            Z.siteDescription = Nothing,
            Z.siteBaseUrl = Nothing
          }
      writeHtmlRoute :: Z.Route s g () -> (s, g) -> Action ()
      writeHtmlRoute r = Rib.writeRoute r . Lucid.renderText . renderPage site r
  void $ Z.generateSite writeHtmlRoute [[relfile|*.md|]]

renderPage :: Z.Site -> Z.Route s g () -> (s, g) -> Html ()
renderPage site r val = html_ [lang_ "en"] $ do
  head_ $ do
    Z.renderRouteHead site r (fst val)
    stylesheet "https://cdn.jsdelivr.net/npm/semantic-ui@2.4.2/dist/semantic.min.css"
    stylesheet "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.11.2/css/all.min.css"
    style_ [type_ "text/css"] $ C.render style
    googleFonts $ [headerFont, bodyFont, monoFont]
  body_ $ do
    div_ [class_ "ui text container", id_ "thesite"] $ do
      br_ mempty
      Z.renderRouteBody r val

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
  "img" ? do
    C.maxWidth $ pct 50
    C.display C.block
    C.marginLeft C.auto
    C.marginRight C.auto
  Z.style
