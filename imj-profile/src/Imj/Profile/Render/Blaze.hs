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
import qualified Data.ByteString as B
import qualified System.IO as IO

import           Text.Blaze.Html5 (Html, ToMarkup(..), docTypeHtml, body, toHtml)
import qualified Text.Blaze.Html.Renderer.Utf8 as Utf8(renderHtmlToByteStringIO)
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
    , "function show_overlay(e){"
    , "  e.childNodes[0].style.display=\"block\""
    , "}"
    , "function hide_overlay(e){"
    , "  e.childNodes[0].style.display=\"none\""
    , "}"
    , "function toggle_details(e,evt){"
    , "  toggleExpand(e,e.childNodes[2],evt)"
    , "}"
    -- using https://stackoverflow.com/a/26476282 for height animation:
    , "const"
    , "toggleExpand = function(ownerElementAbove,element,evt) {"
    , "  var h = Array.prototype.reduce.call(element.childNodes, function(p, c) {return p + (c.offsetHeight || 0);}, 0);"
    , "  var ms = Math.min(1000,Math.ceil(40*Math.sqrt(h)));"
    , "  if (!element.style.height || element.style.height == '0px') {"
    , "      element.style.transitionDuration = ms.toString() + 'ms'"
    , "      element.style.height = h + 'px';"
    , "  } else {"
    , "      mouseY = evt?evt.clientY:(document.documentElement.clientHeight/2)"
    , "      var ownerTop = getCoordsWrtDoc(ownerElementAbove).top"
    , "      if(ownerTop < 20 + pageYOffset) {" -- top is not clearly visible (we use a 20 margin because of the sticky element)
    , "          var target = (ownerTop + getCoords(element).top) / 2"
    , "          scrollTo(target - mouseY, ms);"
    , "      }"
    , "      element.style.height = '0px';"
    , "  }"
    , "}"
    , "const"
    , "getCoordsWrtDoc = function(elem) {"
    , "  let box = elem.getBoundingClientRect();"
    , "  return {"
    , "    top: box.top + pageYOffset,"
    , "    left: box.left + pageXOffset"
    , "  };"
    , "}"
    , "const"
    , "getCoordsWrtScreen = function(elem) {"
    , "  return elem.getBoundingClientRect();"
    , "}"
    , "const"
    , "scrollTo = function(to, duration) {"
    , "    const"
    , "    element = document.scrollingElement || document.documentElement,"
    , "    start = element.scrollTop,"
    , "    change = to - start,"
    , "    startDate = +new Date(),"
    , "    // t = current progress"
    , "    // b = start value"
    , "    // c = change in value"
    , "    // d = duration"
    , "    easeLin = function(t, b, c, d) {"
    , "        return b + c * t / d;"
    , "    },"
    , "    animateScroll = function() {"
    , "        const currentDate = +new Date();"
    , "        const progress = currentDate - startDate;"
    , "        element.scrollTop = parseInt(easeLin(progress, start, change, duration));"
    , "        if(progress < duration) {"
    , "            requestAnimationFrame(animateScroll);"
    , "        }"
    , "        else {"
    , "            element.scrollTop = to;"
    , "        }"
    , "    };"
    , "    animateScroll();"
    , "};"
    ]

resultLine :: ToMarkup a => a -> Maybe Html -> Html
resultLine e =
    maybe
      (H.p txt)
      (\resultDetail ->
        H.div
          H.! A.class_ "clic"
          H.! A.onclick (H.stringValue "toggle_details(this,event)")
          H.! A.onmouseover (H.stringValue "show_overlay(this)")
          H.! A.onmouseout (H.stringValue "hide_overlay(this)")
          -- No title (it is distracting)
          $ do
            H.div -- 0
              H.! A.class_ "overlay" -- has absolute positionning, hence doesn't take space in flow.
              $ pure ()
            H.div -- 1
              txt
            H.div -- 2
              H.! A.class_ "detail"
              $ do
                H.div H.br -- if br is outside this div, its height is not taken into account in toggleExpand
                resultDetail)
 where
  txt = toHtml e

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
  --BL.writeFile utf8path $ Utf8.renderHtml html
  IO.withFile utf8path IO.WriteMode $ \h ->
    Utf8.renderHtmlToByteStringIO (B.hPutStr h) html

--  writeFile (base <> ".string.html") $ Str.renderHtml html
--  writeFile (base <> ".pretty.html") $ Pretty.renderHtml html
  return utf8path
