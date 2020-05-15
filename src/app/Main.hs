{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Clay ((?), Css, em, pct)
import qualified Clay as C
import qualified Data.Text as T
import Development.Shake
import Main.Utf8
import Neuron.CLI (run)
import Neuron.Config (Config)
import qualified Neuron.Config as Config
import Neuron.Web.Generate (generateSite)
import Neuron.Web.Route (Route (..))
import Neuron.Web.View (renderRouteBody, renderRouteHead, style)
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
  let writeHtmlRoute :: Route g a -> (g, a) -> Action ()
      writeHtmlRoute r x = do
        html <- liftIO $ fmap snd $ renderStatic $ renderPage config r x
        -- FIXME: Make rib take bytestrings
        Rib.writeRoute r $ decodeUtf8 @Text html
  void $ generateSite config writeHtmlRoute

renderPage :: PandocBuilder t m => Config -> Route g a -> (g, a) -> m ()
renderPage config r val = elAttr "html" ("lang" =: "en") $ do
  el "head" $ do
    renderRouteHead config r val
    case r of
      Route_Redirect _ ->
        blank
      _ -> do
        forM_
          [ "https://cdn.jsdelivr.net/npm/semantic-ui@2.4.2/dist/semantic.min.css",
            "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.11.2/css/all.min.css"
          ]
          $ \url ->
            elAttr "link" ("rel" =: "stylesheet" <> "href" =: url) blank
        elAttr "style" ("type" =: "text/css") $ text $ toText $ C.renderWith C.compact [] $ mainStyle config
        googleFonts [headerFont, bodyFont, monoFont]
        when (Config.mathJaxSupport config) $
          elAttr "script" ("id" =: "MathJax-script" <> "src" =: "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js" <> "async" =: "") blank
  el "body" $ do
    elAttr "div" ("id" =: "thesite" <> "class" =: "ui text container") $ do
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
  "code, pre, tt, .monoFont" ? do
    C.fontFamily [monoFont, "SFMono-Regular", "Menlo", "Monaco", "Consolas", "Liberation Mono", "Courier New"] [C.monospace]
  style cfg
