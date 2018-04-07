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
    whiteSpace nowrap

  ".stick" ? do
    position sticky
    top $ px 0
    zIndex 2

  ".clic" ? do
    cursor pointer
    position relative -- so that the overlay inside can be absolutely positionned.

  ".overlay" ? do
    display none -- hide by default

    position absolute
    width $ pct 100
    height $ pct 100

    backgroundColor $ rgba 239 239 255 0.05


renderCss :: FilePath -> Css -> IO ()
renderCss path css = do
  createDirectories path
  writeFile path $ render css
