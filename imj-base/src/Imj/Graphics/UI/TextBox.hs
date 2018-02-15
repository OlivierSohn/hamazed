{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}

module Imj.Graphics.UI.TextBox
        ( TextBox(..)
        , mkTextBox
        , addText
        ) where

import           Imj.Prelude
import           Control.Monad(foldM_)

import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.Class.Words
import           Imj.Graphics.Text.ColorString hiding (take)
import           Imj.Graphics.UI.RectContainer
import           Imj.Geo.Discrete
import           Imj.Graphics.Color

-- | The invariant is that no 'Line' should be wider than the width of 'Size'
data TextBox a = TextBox {
    getBox :: {-# UNPACK #-} !Size
  , getLines :: [a]
  -- ^ From bottom to top.
}
instance Positionable (TextBox ColorString) where
  drawAt (TextBox sizeContent@(Size h _) strs) coords = do
    let enclosingContainer = RectContainer sizeContent coords
    drawUsingColor enclosingContainer whiteOnBlack
    let lowerLeftContent = translate coords (Coords (fromIntegral h) 1)
    foldM_
      (\lineStart str -> do
        drawAt str lineStart
        return $ translateInDir Up lineStart
      ) lowerLeftContent strs

  -- | Takes enclosing frame into account
  width (TextBox (Size _ w) _) = 2 + fromIntegral w
  {-# INLINABLE width #-}
  {-# INLINABLE drawAt #-}

mkTextBox :: Size -> TextBox a
mkTextBox = flip TextBox []

{-# INLINABLE addText #-}
addText :: (Words t)
        => t
        -> TextBox t
        -> TextBox t
addText t (TextBox c@(Size h w) l) =
  let tLines = multiLine (fromIntegral w) t
  in TextBox c $ take (fromIntegral h) $ reverse tLines ++ l
