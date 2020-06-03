{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Clay ((?), Css, em)
import qualified Clay as C
import Control.Monad.Reader
import qualified Data.Text as T
import Development.Shake
import Main.Utf8
import Neuron.CLI (run)
import Neuron.Config (Config)
import qualified Neuron.Config as Config
import Neuron.Web.Generate (generateSite)
import Neuron.Web.Generate.Route (staticRouteConfig)
import Neuron.Web.Route (NeuronWebT, Route (..), RouteError, runNeuronWeb)
import Neuron.Web.View (renderRouteBody, renderRouteHead, style)
import Neuron.Zettelkasten.Graph.Type (ZettelGraph)
import Reflex.Dom.Core
import Reflex.Dom.Pandoc.Document (PandocBuilder)
import Relude
import qualified Rib

main :: IO ()
main = withUtf8 $ run generateMainSite

generateMainSite :: Action ()
generateMainSite = do
  Rib.buildStaticFiles ["static/**"]
  config <- Config.getConfig
  let writeHtmlRoute :: Route a -> (ZettelGraph, a) -> Action (RouteError a)
      writeHtmlRoute r x = do
        (errors, html) <- liftIO $ renderStatic $ do
          runNeuronWeb staticRouteConfig $
            renderPage config r x
        -- FIXME: Make rib take bytestrings
        Rib.writeRoute r $ decodeUtf8 @Text html
        pure errors
  void $ generateSite config writeHtmlRoute

renderPage :: PandocBuilder t m => Config -> Route a -> (ZettelGraph, a) -> NeuronWebT t m (RouteError a)
renderPage config r val = elAttr "html" ("lang" =: "en") $ do
  el "head" $ do
    renderRouteHead config r val
    case r of
      Route_Redirect _ ->
        blank
      _ -> do
        let semanticCss = "https://cdn.jsdelivr.net/npm/fomantic-ui@2.8.4/dist/semantic.min.css"
        elAttr "link" ("rel" =: "stylesheet" <> "href" =: semanticCss) blank
        elAttr "style" ("type" =: "text/css") $ text $ toText $ C.renderWith C.compact [] $ mainStyle config
        googleFonts [headerFont, bodyFont, monoFont]
        when (Config.mathJaxSupport config) $
          elAttr "script" ("id" =: "MathJax-script" <> "src" =: "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js" <> "async" =: "") blank
  el "body" $ do
    elAttr "div" ("id" =: "thesite" <> "class" =: "ui fluid container") $ do
      renderRouteBody config r val
  where
    googleFonts :: DomBuilder t m => [Text] -> m ()
    googleFonts fs =
      let fsEncoded = T.intercalate "|" $ T.replace " " "+" <$> fs
          fsUrl = "https://fonts.googleapis.com/css?family=" <> fsEncoded <> "&display=swap"
       in elAttr "link" ("rel" =: "stylesheet" <> "href" =: fsUrl) blank

headerFont :: Text
headerFont = "DM Serif Text"

bodyFont :: Text
bodyFont = "DM Sans"

monoFont :: Text
monoFont = "DM Mono"

mainStyle :: Config -> Css
mainStyle cfg = do
  "body" ? do
    C.important $ C.backgroundColor "#eee"
  "div#thesite" ? do
    C.fontFamily [bodyFont] [C.serif]
    C.paddingBottom $ em 1
    "h1, h2, h3, h4, h5, h6, .ui.header, .headerFont" ? do
      C.fontFamily [headerFont] [C.sansSerif]
    "code, pre, tt, .monoFont" ? do
      C.fontFamily [monoFont, "SFMono-Regular", "Menlo", "Monaco", "Consolas", "Liberation Mono", "Courier New"] [C.monospace]
    style cfg
