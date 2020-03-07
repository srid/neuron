{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Rib (IsRoute)
import Rib.Extra.CSS (googleFonts, stylesheet)
import Text.Pandoc.Highlighting (styleToCss, tango)

data Route a where
  Route_Zettel :: Z.Route s g () -> Route (s, g)

instance IsRoute Route where
  routeFile = \case
    Route_Zettel r ->
      Rib.routeFile r

main :: IO ()
main = Z.run (thisDir </> [reldir|content|]) (thisDir </> [reldir|dest|]) generateSite
  where
    thisDir = [reldir|example/srid.ca|]

generateSite :: Action ()
generateSite = do
  let writeHtmlRoute r = Rib.writeRoute r . Lucid.renderText . renderPage r
  void $ Z.generateSite (writeHtmlRoute . Route_Zettel) [[relfile|*.md|]]

routeTitle :: a -> Route a -> Text
routeTitle val =
  maybe siteTitle (<> " - " <> siteTitle) . \case
    Route_Zettel r ->
      Z.routeTitle (fst val) r
  where
    siteTitle = "Srid's new website"

renderPage :: Route a -> a -> Html ()
renderPage route val = with html_ [lang_ "en"] $ do
  head_ $ do
    meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    -- TODO: open graph
    title_ $ toHtml $ routeTitle val route
    stylesheet "https://cdn.jsdelivr.net/npm/semantic-ui@2.4.2/dist/semantic.min.css"
    stylesheet "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.11.2/css/all.min.css"
    style_ [type_ "text/css"] $ styleToCss tango
    style_ [type_ "text/css"] $ C.render style
    googleFonts $ [headerFont, bodyFont, monoFont]
  body_ $ do
    div_ [class_ "ui text container", id_ "thesite"] $ do
      br_ mempty
      case route of
        Route_Zettel r ->
          Z.renderRoute r val

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
  -- TODO: Move these to neuron's css
  "div.connections" ? do
    "a" ? do
      C.important $ color white
    "a:hover" ? do
      C.opacity 0.5
