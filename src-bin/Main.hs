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
-- TODO: Don't expose every module

import Development.Shake
import Lucid
import Main.Utf8
import qualified Neuron.Config as Z
import qualified Neuron.Web.Generate as Z
import qualified Neuron.Web.Route as Z
import qualified Neuron.Web.View as Z
import Neuron.CLI (run)
import Relude
import qualified Rib
import Rib.Extra.CSS (googleFonts, stylesheet)

main :: IO ()
main = withUtf8 $ run generateSite

generateSite :: Action ()
generateSite = do
  Rib.buildStaticFiles ["static/**"]
  config <- Z.getConfig
  let writeHtmlRoute :: Z.Route s g a -> (s, g, a) -> Action ()
      writeHtmlRoute r = Rib.writeRoute r . Lucid.renderText . renderPage config r
  void $ Z.generateSite config writeHtmlRoute ["*.md"]

renderPage :: Z.Config -> Z.Route s g a -> (s, g, a) -> Html ()
renderPage config r val@(s, _, _) = html_ [lang_ "en"] $ do
  head_ $ do
    Z.renderRouteHead config r s
    case r of
      Z.Route_Redirect _ ->
        mempty
      _ -> do
        stylesheet "https://cdn.jsdelivr.net/npm/semantic-ui@2.4.2/dist/semantic.min.css"
        stylesheet "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.11.2/css/all.min.css"
        style_ [type_ "text/css"] $ C.renderWith C.compact [] $ style config
        googleFonts [headerFont, bodyFont, monoFont]
        when (Z.mathJaxSupport config) $
          with (script_ mempty) [id_ "MathJax-script", src_ "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js", async_ ""]
  body_ $ do
    div_ [class_ "ui text container", id_ "thesite"] $ do
      Z.renderRouteBody config r val

headerFont :: Text
headerFont = "Oswald"

bodyFont :: Text
bodyFont = "Open Sans"

monoFont :: Text
monoFont = "Roboto Mono"

style :: Z.Config -> Css
style cfg = "div#thesite" ? do
  C.fontFamily [bodyFont] [C.serif]
  C.paddingTop $ em 1
  C.paddingBottom $ em 1
  "p" ? do
    C.lineHeight $ pct 150
  "h1, h2, h3, h4, h5, h6, .ui.header" ? do
    C.fontFamily [headerFont] [C.sansSerif]
  "img" ? do
    C.maxWidth $ pct 50
    C.display C.block
    C.marginLeft C.auto
    C.marginRight C.auto
  Z.style cfg
