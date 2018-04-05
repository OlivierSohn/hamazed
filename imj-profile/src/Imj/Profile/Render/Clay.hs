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

mkCss :: Css
mkCss = "*" ? do -- * to apply the properties to all content type
  font ( Optional Nothing Nothing Nothing
       , Required (px 12) Nothing [] [monospace]
       )
  margin auto auto auto auto
  textAlign center

renderCss :: FilePath -> Css -> IO ()
renderCss path css = writeFile path $ render css
