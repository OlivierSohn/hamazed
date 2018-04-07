{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.Profile.Render
    ( renderResultsHtml
    -- reexport
    , FilePath
    ) where

import           Imj.Prelude
import           Prelude(FilePath)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import           Imj.Graphics.Text.ColorString

import           Imj.Profile.Render.Blaze
import           Imj.Profile.Render.Clay

renderResultsHtml :: (ToMarkup a) => [(a, Maybe [[ColorString]])] -> IO FilePath
renderResultsHtml resultsAndSubresults = do
  let cssName = "results.css"
  renderCss ("report/" <> cssName) mkCss
  resultsAndLinks <-
    forM
      (zip [0 :: Int ..] resultsAndSubresults)
      (\(i,(a,mayDetailsContents)) ->
        (,) a <$> maybe
          (return Nothing)
          (\detailsContents -> do
            let detailName = show i
            _ <- renderHtml ("report/html/results/" ++ show i)
              (fromHeaderBody
                (do
                  H.title $ H.string $ "Test detail #" ++ show i
                  cssHeader $ "../../" <> cssName) $
                    mconcat <$> forM detailsContents (\lines -> do
                      H.div $ mconcat <$> forM lines (H.p . H.toHtml)
                      H.br))
            return $ Just $ "results/" ++ detailName ++ ".html")
          mayDetailsContents)

  renderHtml "report/html/results" $
    fromHeaderBody
      (do
        H.title "Test report"
        H.meta H.! A.httpEquiv "refresh" H.! A.content "2"
        cssHeader $ "../" <> cssName
        scripts) $
      mconcat $ map (uncurry resultLine) resultsAndLinks
