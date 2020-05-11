{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Clay hiding (s, style, type_)
import qualified Clay as C
import Development.Shake
import Lucid
import Main.Utf8
import Neuron.CLI (run)
import Neuron.Config (Config)
import qualified Neuron.Config as Config
import Neuron.Web.Generate (generateSite)
import Neuron.Web.Route (Route (..))
import Neuron.Web.View (renderRouteBody, renderRouteHead, style)
import Relude
import qualified Rib
import Rib.Extra.CSS (googleFonts, stylesheet)

main :: IO ()
main = withUtf8 $ run generateMainSite

generateMainSite :: Action ()
generateMainSite = do
  Rib.buildStaticFiles ["static/**"]
  config <- Config.getConfig
  let writeHtmlRoute :: Route g a -> (g, a) -> Action ()
      writeHtmlRoute r = Rib.writeRoute r . Lucid.renderText . renderPage config r
  void $ generateSite config writeHtmlRoute

renderPage :: Config -> Route g a -> (g, a) -> Html ()
renderPage config r val = html_ [lang_ "en"] $ do
  head_ $ do
    renderRouteHead config r val
    case r of
      Route_Redirect _ ->
        mempty
      _ -> do
        stylesheet "https://cdn.jsdelivr.net/npm/semantic-ui@2.4.2/dist/semantic.min.css"
        stylesheet "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.11.2/css/all.min.css"
        style_ [type_ "text/css"] $ C.renderWith C.compact [] $ mainStyle config
        googleFonts [headerFont, bodyFont, monoFont]
        when (Config.mathJaxSupport config) $
          with (script_ mempty) [id_ "MathJax-script", src_ "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js", async_ ""]
  body_
    $ div_ [class_ "ui text container", id_ "thesite"]
    $ renderRouteBody config r val

headerFont :: Text
headerFont = "DM Serif Text"

bodyFont :: Text
bodyFont = "DM Sans"

monoFont :: Text
monoFont = "DM Mono"

mainStyle :: Config -> Css
mainStyle cfg = "div#thesite" ? do
  C.fontFamily [bodyFont] [C.serif]
  C.paddingTop $ em 1
  C.paddingBottom $ em 1
  "p" ? do
    C.lineHeight $ pct 150
  "h1, h2, h3, h4, h5, h6, .ui.header, .headerFont" ? do
    C.fontFamily [headerFont] [C.sansSerif]
  "img" ? do
    C.maxWidth $ pct 100 -- Prevents large images from overflowing beyond zettel borders
  "code, pre, tt" ? do
    fontFamily [monoFont, "SFMono-Regular", "Menlo", "Monaco", "Consolas", "Liberation Mono", "Courier New"] [monospace]
  style cfg
