{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.Profile.Render.Blaze
    ( renderHtml
    , fromHeaderBody
    , cssHeader
    , scripts
    , resultLine
    , ToMarkup
    ) where

import           Imj.Prelude
import           Prelude(FilePath, unlines)
import qualified Data.ByteString.Lazy as BS(writeFile)
--import           Data.Char(chr)

import           Text.Blaze.Html5 (Html, ToMarkup(..), docTypeHtml, body, toHtml)
import qualified Text.Blaze.Html.Renderer.Utf8 as Utf8(renderHtml)
--import qualified Text.Blaze.Html.Renderer.String as Str(renderHtml)
--import qualified Text.Blaze.Html.Renderer.Pretty as Pretty(renderHtml)
import qualified Text.Blaze.Html5 as H
import           Text.Blaze.Html4.FrameSet ((!))
import qualified Text.Blaze.Html5.Attributes as A

import           Imj.File

fromHeaderBody :: Html -> Html -> Html
fromHeaderBody h b = docTypeHtml $ do
  H.head h
  body b

cssHeader :: FilePath -> Html
cssHeader css =
  H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href (H.stringValue css)
scripts :: Html
scripts =
  H.script ! A.lang "javascript" $ H.string $ unlines
    [ ""
    , "function to_open(url){"
    , "var viewportwidth = document.documentElement.clientWidth;"
    , "var viewportheight = document.documentElement.clientHeight;"
    , "window.resizeBy(-500,0);"
    , "window.moveTo(0,0);"
    , "tm=window.open(url,\"Test details\",\"width=500,height =\"+(viewportheight)+\",top=0,left=\"+viewportwidth+\"\");"
    , "}"
    , "function to_close(){"
    , "tm.close();"
    , "}"
    ]

resultLine :: ToMarkup a => a -> Maybe String -> Html
resultLine e subWindow =
  pMayTitle $ toHtml e
 where
  pMayTitle = maybe
    H.p
    (\w -> H.p H.! A.onclick (H.stringValue $ "to_open(\"" ++ w ++ "\")"))
    subWindow

--    ((H.!) H.p . A.title . H.stringValue . replaceNewlines)
{-  replaceNewlines [] = []
  replaceNewlines ('\n':xs) = chr 13:replaceNewlines xs
  replaceNewlines (c:xs)    = c     :replaceNewlines xs
  -}
  -- in a <p title="titleTxt"> ... </p>, onChrome, any of &#10; &#13; &#xA; in titleTxt make newlines.

renderHtml :: FilePath -> Html -> IO FilePath
renderHtml base html = do
  createDirectories base
  let utf8path = base <> ".html"
  BS.writeFile utf8path $ Utf8.renderHtml html
--  writeFile (base <> ".string.html") $ Str.renderHtml html
--  writeFile (base <> ".pretty.html") $ Pretty.renderHtml html
  return utf8path
