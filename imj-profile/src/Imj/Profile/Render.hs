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
  detailPages >>= mainPage
 where
  dir = "report"

  css = renderCss (dir <> "/" <> cssName) mkCss

  mainPage resultsAndLinks = renderHtml (dir <> "/html/results") $
    fromHeaderBody
      (do
        pagetTitle "Test report"
--        meta ! A.httpEquiv "refresh" ! A.content "2"
        cssHeader $ "../" <> cssName
        scripts)
      (do
        testStatus
        mconcat $ List.map (uncurry resultLine) resultsAndLinks)

  detailPages =
    forM (zip [0 :: Int ..] resultsAndSubresults)
      (\(idx,(res,mayDetailsContents)) ->
        (,) res <$> maybe
          (return Nothing)
          (\detailsContents -> do
            let detailName = show idx
            _ <- renderHtml (dir <> "/html/results/" <> detailName)
              (fromHeaderBody
                (do
                  pagetTitle "Test detail"
                  cssHeader $ "../../" <> cssName)
                (do
                  testStatus
                  mconcat <$> forM detailsContents (\lines -> do
                    div $ mconcat <$> forM lines (\l -> p $ toHtml $ bool l "|" $ empty l)
                    br)))
            return $ Just $ "results/" ++ detailName ++ ".html")
          mayDetailsContents)

  testStatus = do
    br
    div
      ! A.style (colorAttribute statusColor)
      ! A.class_ "stick"
      $ string status
    br
   where
    statusColor = LayeredColor (gray 13) black

  pagetTitle x = title $ string $ x ++ " - " ++ status

  cssName = "results.css"
