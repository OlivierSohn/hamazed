{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.Profile.Render
    ( renderResultsHtml
    -- reexport
    , FilePath
    ) where

import           Imj.Prelude hiding(div)
import           Prelude(FilePath)

import qualified Data.List as List(map)
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as A

import           Imj.File
import           Imj.Graphics.Color.Types
import           Imj.Graphics.Text.ColorString
import           Imj.Graphics.Class.Words

import           Imj.Profile.Render.Blaze
import           Imj.Profile.Render.Clay

renderResultsHtml :: (ToMarkup a)
                  => String
                  -> [(a, Maybe [[ColorString]])]
                  -> IO FilePath
renderResultsHtml status resultsAndSubresults = do
  renameDirectoryIfExists dir
  css
  mainPage resultDetails
 where
  dir = "report"

  css = renderCss (dir <> "/" <> cssName) mkCss

  mainPage resultsAndLinks = renderHtml (dir <> "/html/results") $
    fromHeaderBody
      (do
        pageTitle "Test report"
--        meta ! A.httpEquiv "refresh" ! A.content "2"
        cssHeader $ "../" <> cssName
        scripts)
      (do
        testStatus
        mconcat $ List.map (uncurry resultLine) resultsAndLinks)

  resultDetails =
    List.map
      (\(_,(res,mayDetailsContents)) ->
        (,) res $ maybe
          Nothing
          (Just . mconcat . List.map (\lines ->
              div $ do
                mconcat $ List.map (\l -> p $ toHtml $ bool l "|" $ empty l) lines
                br
              ))
          mayDetailsContents)
    (zip [0 :: Int ..] resultsAndSubresults)

  testStatus = do
    br
    div
      ! A.style (colorAttribute statusColor)
      ! A.class_ "stick"
      $ string status
    br
   where
    statusColor = LayeredColor (gray 13) black

  pageTitle x = title $ string x

  cssName = "results.css"
