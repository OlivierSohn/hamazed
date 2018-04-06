{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.Profile.Render.Blaze
    ( renderHtml
    , fromHeaderBody
    , cssHeader
    , resultLine
    , ToMarkup
    ) where

import           Imj.Prelude
import           Prelude(FilePath, writeFile)
import qualified Data.ByteString.Lazy as BS(writeFile)

import           Text.Blaze.Html5 (Html, ToMarkup(..), docTypeHtml, body, toHtml)
import qualified Text.Blaze.Html.Renderer.Utf8 as Utf8(renderHtml)
import qualified Text.Blaze.Html.Renderer.String as Str(renderHtml)
import qualified Text.Blaze.Html.Renderer.Pretty as Pretty(renderHtml)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html4.FrameSet ((!))
import qualified Text.Blaze.Html5.Attributes as A

fromHeaderBody :: Html -> Html -> Html
fromHeaderBody h b = docTypeHtml $ do
  H.head h
  body b

cssHeader :: FilePath -> Html
cssHeader css = do
  H.title "Simple"
  H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href (H.stringValue css)

resultLine :: ToMarkup a => a -> Html
resultLine e = do
  toHtml e
  H.br

renderHtml :: FilePath -> Html -> IO ()
renderHtml path html = do
  BS.writeFile ("z.utf8." <> path) $ Utf8.renderHtml html
  writeFile ("z.string." <> path) $ Str.renderHtml html
  writeFile ("z.pretty." <> path) $ Pretty.renderHtml html
