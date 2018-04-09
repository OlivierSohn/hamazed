{-# LANGUAGE NoImplicitPrelude #-}
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
    zIndex 2 -- so that it is not hidden behind elements coming after.

  ".clic" ? do
    position relative -- so that the overlay inside can be absolutely positionned.
    cursor pointer

  ".overlay" ? do
    position absolute
    width $ pct 100
    height $ pct 100
    zIndex 1

    display none -- hide by default
    backgroundColor $ rgba 239 239 255 0.05


  ".detail" ? do -- height and transition duration are overriden in 'toggleExpand'
    overflow hidden
    height $ px 0
    transition "height" 1 linear 0 -- same easing as for scroll in 'toggleExpand'.

renderCss :: FilePath -> Css -> IO ()
renderCss path css = do
  createDirectories path
  writeFile path $ render css
