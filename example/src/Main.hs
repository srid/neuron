{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Clay ((?), Css, em, pc, px, sym)
import qualified Clay as C
import Control.Monad
import Data.Aeson (FromJSON, fromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import Data.Text (Text)
import Development.Shake
import GHC.Generics (Generic)
import Lucid
-- import qualified Neuron.Zettelkasten as Z
import Path
import Rib (IsRoute, MMark)
import qualified Rib
import qualified Rib.Parser.MMark as MMark

-- | Route corresponding to each generated static page.
--
-- The `a` parameter specifies the data (typically Markdown document) used to
-- generated the final page text.
data Route a where
  Route_Index :: Route ()
  Route_Article :: ArticleRoute a -> Route a

-- | You may even have sub routes.
data ArticleRoute a where
  ArticleRoute_Index :: ArticleRoute [(Route MMark, MMark)]
  ArticleRoute_Article :: Path Rel File -> ArticleRoute MMark

-- | The `IsRoute` instance allows us to determine the target .html path for
-- each route. This affects what `routeUrl` will return.
instance IsRoute Route where
  routeFile = \case
    Route_Index ->
      pure [relfile|index.html|]
    Route_Article r ->
      fmap ([reldir|article|] </>) $ case r of
        ArticleRoute_Article srcPath ->
          replaceExtension ".html" srcPath
        ArticleRoute_Index ->
          pure [relfile|index.html|]

-- | Main entry point to our generator.
--
-- `Rib.run` handles CLI arguments, and takes three parameters here.
--
-- 1. Directory `content`, from which static files will be read.
-- 2. Directory `dest`, under which target files will be generated.
-- 3. Shake action to run.
--
-- In the shake action you would expect to use the utility functions
-- provided by Rib to do the actual generation of your static site.
main :: IO ()
main = Rib.run [reldir|example/content|] [reldir|example/dest|] generateSite

-- | Shake action for generating the static site
generateSite :: Action ()
generateSite = do
  -- Copy over the static files
  Rib.buildStaticFiles [[relfile|static/**|]]
  let writeHtmlRoute :: Route a -> a -> Action ()
      writeHtmlRoute r = Rib.writeRoute r . Lucid.renderText . renderPage r
  -- Build individual sources, generating .html for each.
  articles <-
    Rib.forEvery [[relfile|*.md|]] $ \srcPath -> do
      let r = Route_Article $ ArticleRoute_Article srcPath
      doc <- MMark.parse srcPath
      writeHtmlRoute r doc
      pure (r, doc)
  writeHtmlRoute (Route_Article ArticleRoute_Index) articles
  writeHtmlRoute Route_Index ()

-- | Define your site HTML here
renderPage :: Route a -> a -> Html ()
renderPage route val = with html_ [lang_ "en"] $ do
  head_ $ do
    meta_ [httpEquiv_ "Content-Type", content_ "text/html; charset=utf-8"]
    title_ $ routeTitle
    style_ [type_ "text/css"] $ C.render pageStyle
  body_ $ do
    with div_ [id_ "thesite"] $ do
      with div_ [class_ "header"] $
        with a_ [href_ "/"] "Back to Home"
      h1_ routeTitle
      case route of
        Route_Index ->
          p_ $ do
            "This site is work in progress. Meanwhile visit the "
            with a_ [href_ $ Rib.routeUrl $ Route_Article ArticleRoute_Index] "articles"
            " page."
        Route_Article ArticleRoute_Index ->
          div_ $ forM_ val $ \(r, src) ->
            with li_ [class_ "pages"] $ do
              let meta = getMeta src
              b_ $ with a_ [href_ (Rib.routeUrl r)] $ toHtml $ title meta
              maybe mempty renderMarkdown $ description meta
        Route_Article (ArticleRoute_Article _) ->
          with article_ [class_ "post"] $ do
            MMark.render val
  where
    routeTitle :: Html ()
    routeTitle = case route of
      Route_Index -> "Neuron sample site"
      Route_Article (ArticleRoute_Article _) -> toHtml $ title $ getMeta val
      Route_Article ArticleRoute_Index -> "Articles"
    renderMarkdown =
      MMark.render . either (error . T.unpack) id . MMark.parsePure "<none>"

-- | Define your site CSS here
pageStyle :: Css
pageStyle = "div#thesite" ? do
  C.margin (em 4) (pc 20) (em 1) (pc 20)
  ".header" ? do
    C.marginBottom $ em 2
  "li.pages" ? do
    C.listStyleType C.none
    C.marginTop $ em 1
    "b" ? C.fontSize (em 1.2)
    "p" ? sym C.margin (px 0)

-- | Metadata in our markdown sources
data SrcMeta
  = SrcMeta
      { title :: Text,
        -- | Description is optional, hence `Maybe`
        description :: Maybe Text
      }
  deriving (Show, Eq, Generic, FromJSON)

-- | Get metadata from Markdown's YAML block
getMeta :: MMark -> SrcMeta
getMeta src = case MMark.projectYaml src of
  Nothing -> error "No YAML metadata"
  Just val -> case fromJSON val of
    Aeson.Error e -> error $ "JSON error: " <> e
    Aeson.Success v -> v
