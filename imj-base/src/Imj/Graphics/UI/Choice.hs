{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Graphics.UI.Choice
        ( Choice(..)
        , setDownUpChar
        ) where

import           Imj.Prelude

import qualified Data.Text as Text(length)
import           Data.Text(pack)
import           Imj.Geo.Discrete
import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.Color
import           Imj.Graphics.UI.Colored

{- | Chosing between multiple values
-}
data Choice = Choice {
    choiveValue :: !Text
  , choiceDownChar, choiceUpChar :: {-# UNPACK #-} !Char
  , choiceColor :: {-# UNPACK #-} !LayeredColor
}

setDownUpChar :: Char -> Char -> Choice -> Choice
setDownUpChar d u (Choice v _ _ c) = Choice v d u c

{- | Example:

@
      interpolation : < Linear >
@
-}
instance Positionable Choice where
  drawAt (Choice v up down color) coords = do
    case down of
      ' ' -> return ()
      _ -> drawAt (Colored color downTxt) coords
    drawAt (Colored color v) $ move (Text.length downTxt) RIGHT coords
    case up of
      ' ' -> return ()
      _ -> drawAt (Colored color upTxt) $ move (Text.length downTxt + Text.length v) RIGHT coords

   where

    downTxt = pack [down, ' ']
    upTxt = pack [' ', up]

  width (Choice v _ _ _) = fromIntegral $ 4 + Text.length v

  height = const 1

  {-# INLINABLE width #-}
  {-# INLINABLE height #-}
  {-# INLINABLE drawAt #-}
