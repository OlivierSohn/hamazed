{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Imj.Profile.Render
    ( renderResults
    ) where

import           Imj.Prelude

import           Imj.Profile.Render.Blaze
import           Imj.Profile.Render.Clay
--import           Text.Blaze.Html5

renderResults :: (ToMarkup a) => [a] -> IO ()
renderResults allResultsCS = do
  let cssFile = "results.css"
  renderCss cssFile mkCss
  renderHtml "results.html" $
    fromHeaderBody
      (cssHeader cssFile) $
      mconcat $ map resultLine allResultsCS
