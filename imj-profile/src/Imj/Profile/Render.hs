{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.Profile.Render
    ( writeHtmlReport
    , resultsToHtml
    -- reexport
    , FilePath
    ) where

import           Imj.Prelude hiding(div)

import           Data.Text(pack)
import           Data.UUID(UUID)
import           Text.Blaze.Html5
import qualified Text.Blaze.Html5.Attributes as A

import           Imj.Graphics.Color.Types

import           Imj.File
import           Imj.Graphics.Text.ColorString
import           Imj.Profile.Intent
import           Imj.Profile.Render.Blaze
import           Imj.Profile.Render.Clay
import           Imj.Profile.Results


writeHtmlReport :: UUID
                -> Html
                -> UserIntent
                -> IO ()
writeHtmlReport key h intent = do
  name <- renderResultsHtml key intentStr h
  putStrLn $ colored ("Wrote Chrome-compatible html report: " <> pack name) yellow

 where

  intentStr = case intent of
    Cancel -> "The test was interrupted."
    Report _ -> "The test is still running."
    Run -> "The test has finished." -- because we write a report only at the end, or on 'Report'
    Pause _ -> "The test is paused" -- should not happen


renderResultsHtml :: UUID
                  -> String
                  -> Html
                  -> IO FilePath
renderResultsHtml k status resultsAndSubresults = do
  deleteOrRename dir k
  renderCss (dir <> "/" <> cssName) mkCss
  renderHtml (dir <> "/html/results") $
    fromHeaderBody
      (do
        pageTitle "Test report"
--        meta ! A.httpEquiv "refresh" ! A.content "2"
        cssHeader $ "../" <> cssName
        scripts)
      (do
        bodyHeader
        resultsAndSubresults)
 where
  dir = "report"

  bodyHeader = do
    div
      ! A.style (colorAttribute msgColor)
      $ do
        br
        p "Chrome-compatible html report."
        br
    div
      ! A.style (colorAttribute statusColor)
      ! A.class_ "stick"
      $ string status
    div br
   where
    statusColor = LayeredColor (gray 13) black
    msgColor = LayeredColor (gray 3) (gray 13)

  pageTitle x = title $ string x

  cssName = "results.css"
