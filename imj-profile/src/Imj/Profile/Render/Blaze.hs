{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Imj.Profile.Render.Blaze
    ( renderHtml
    , fromHeaderBody
    , cssHeader
    , scripts
    , ToMarkup
    ) where

import           Imj.Prelude
import           Prelude(FilePath)
import qualified Data.ByteString as B
import           Data.FileEmbed(embedStringFile)
import qualified System.IO as IO

import qualified Text.Blaze.Html.Renderer.Utf8 as Utf8
import           Text.Blaze.Html5
import           Text.Blaze.Html4.FrameSet ((!))
import qualified Text.Blaze.Html5.Attributes as A

import           Imj.File

fromHeaderBody :: Html -> Html -> Html
fromHeaderBody ht bo = docTypeHtml $ do
  head ht
  body bo

cssHeader :: FilePath -> Html
cssHeader css =
  link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href (stringValue css)
scripts :: Html
scripts =
  script ! A.lang "javascript" $ string ($(embedStringFile "js/results.js"))

renderHtml :: FilePath -> Html -> IO FilePath
renderHtml name h = do
  createDirectories name
  let utf8path = name <> ".html"
  IO.withFile utf8path IO.WriteMode $ \str ->
    Utf8.renderHtmlToByteStringIO (B.hPutStr str) h
  return utf8path
