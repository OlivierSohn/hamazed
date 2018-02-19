{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}

module Imj.Graphics.UI.TextBox
        ( TextBox(..)
        , mkTextBox
        , HeightAdjustPolicy(..)
        , mkAdjustableHeightTextBox
        , addText
        ) where

import           Imj.Prelude
import           Prelude(length)
import           Control.Monad(foldM_)

import           Imj.Geo.Discrete
import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.Class.Words
import           Imj.Graphics.Color
import           Imj.Graphics.Render.FromMonadReader
import           Imj.Graphics.UI.RectArea
import           Imj.Graphics.UI.RectContainer

-- | The invariant is that no 'Line' should be wider than the width of 'Size'
data TextBox a = TextBox {
    getBox :: {-# UNPACK #-} !Size
  , getLines :: [a]
  -- ^ From bottom to top.
  , adjustment :: {-unpack sum-} !HeightAdjustPolicy
}
instance Positionable (TextBox ColorString) where
  drawAt (TextBox sizeContent@(Size h _) strs _) coords = do
    let enclosingContainer = RectContainer sizeContent coords
    drawUsingColor enclosingContainer whiteOnBlack
    let upperRightContent = translate coords $ Coords 1 1
        lowerLeftContent = translate coords $ Coords (fromIntegral h) 1
    usingScissor (mkRectArea upperRightContent sizeContent) $
      fill ' ' whiteOnBlack
    foldM_
      (\lineStart str -> do
        drawAt str lineStart
        return $ translateInDir Up lineStart
      ) lowerLeftContent strs

  -- | Takes enclosing frame into account
  width (TextBox (Size _ w) _ _) = 2 + fromIntegral w
  height (TextBox (Size h _) _ _) = 2 + fromIntegral h
  {-# INLINABLE width #-}
  {-# INLINABLE height #-}
  {-# INLINABLE drawAt #-}

data HeightAdjustPolicy =
    NoAdjustment
  | Adjust

mkAdjustableHeightTextBox :: Length Width -> TextBox a
mkAdjustableHeightTextBox w = TextBox (Size 0 w) [] Adjust

mkTextBox :: Size -> TextBox a
mkTextBox s = TextBox s [] NoAdjustment

{-# INLINABLE addText #-}
addText :: (Words t)
        => t
        -> TextBox t
        -> TextBox t
addText t (TextBox s@(Size h w) l adjustHeight) =
  let tLines = multiLine (fromIntegral w) t
      newLines = reverse tLines ++ l
  in case adjustHeight of
        NoAdjustment -> TextBox s (take (fromIntegral h) newLines) adjustHeight
        Adjust -> let newH = fromIntegral $ Prelude.length newLines
                  in TextBox (Size newH w) newLines adjustHeight
