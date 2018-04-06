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
mkCss = "*" ? do -- to apply the properties to all content type
  font ( Optional Nothing Nothing Nothing
       , Required (px 12) Nothing [] [monospace]
       )
  margin auto auto auto auto
  textAlign center
  backgroundColor "#1d1d1d"

renderCss :: FilePath -> Css -> IO ()
renderCss path css = do
  createDirectories path
  writeFile path $ render css
