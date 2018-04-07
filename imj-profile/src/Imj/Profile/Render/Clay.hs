{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.Profile.Render.Clay
    ( mkCss
    , renderCss
    ) where

import           Imj.Prelude
import           Prelude(FilePath)
import           Data.Text.Lazy.IO(writeFile)

import           Clay

import           Imj.File

mkCss :: Css
mkCss = do
  "*" ? do -- to apply the properties to all content type
    font ( Optional Nothing Nothing Nothing
         , Required (px 12) Nothing [] [monospace]
         )
    margin auto auto auto auto
    textAlign center
    backgroundColor "#1d1d1d"

  ".overlay" ? do
    width $ pct 100
    height $ px 10
    position relative
    zIndex 2
    top $ px 0
    left $ px 0
    backgroundColor $ rgba 255 255 255 0.5

  ".clic" ?
    cursor pointer

  ".stick" ? do
    position sticky
    top $ px 0

renderCss :: FilePath -> Css -> IO ()
renderCss path css = do
  createDirectories path
  writeFile path $ render css
