{-# LANGUAGE NoImplicitPrelude #-}

module Color (
    colorFromFrame
) where

import           Imajuscule.Prelude

import           System.Console.ANSI.Codes( color8ToCode )
import           System.Console.ANSI(Color8Code(..), Color8(..))

import           Animation.Types


colorFromFrame :: Frame -> Color8Code
colorFromFrame (Frame f) = color8ToCode $ RGB8Color r g b
  where
    r = 4
    g = (0 + quot f 6) `mod` 2 -- [0..1] , slow changes
    b = (0 + quot f 3) `mod` 3 -- [0..2] , 2x faster changes
