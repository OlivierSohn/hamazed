module Test.Imj.RectArea
         ( testRectArea
         ) where

import           Imj.Prelude

import           Control.Monad(when)

import           Imj.Geo.Discrete
import           Imj.Graphics.UI.RectArea

-- | returns True on success, else errors
testRectArea :: IO Bool
testRectArea = do
  let emptyArea1 = RectArea (Coords 1 1) (Coords 0 1)
      emptyArea2 = RectArea (Coords 1 1) (Coords 1 0)
      emptyArea3 = RectArea (Coords 1 1) (Coords 0 0)
      square1x1 = RectArea (Coords 1 1) (Coords 1 1)
      square2x2 = RectArea (Coords 1 1) (Coords 2 2)
      square3x3 = RectArea (Coords 0 0) (Coords 2 2)
      rect2x3 = RectArea (Coords 1 1) (Coords 2 3)

  when (isEmpty square1x1) $ error "square1x1 is empty"
  when (isEmpty square2x2) $ error "square2x2 is empty"
  unless (isEmpty emptyArea1) $ error "emptyArea1 is not empty"
  unless (isEmpty emptyArea2) $ error "emptyArea2 is not empty"
  unless (isEmpty emptyArea3) $ error "emptyArea3 is not empty"

  unless (growRectArea 1 square1x1 == square3x3) $ error "wrong growth"

  unless (rectAreaSize emptyArea1 == Size 0 0) $ error "wrong empty size"
  unless (rectAreaSize square1x1 == Size 1 1) $ error "wrong 1x1 size"
  unless (rectAreaSize rect2x3 == Size 2 3) $ error "wrong 2x3 size"

  unless (rectAreaCenter square1x1 == Coords 1 1) $ error "wrong 1x1 center"

  unless (intersection rect2x3 rect2x3 == rect2x3) $ error "wrong 2x3 2x3 intersection"
  unless (isEmpty $ intersection emptyArea1 emptyArea1) $ error "wrong 1x1 empty intersection"

  unless (intersection square1x1 square2x2 == square1x1) $ error "wrong 1x1 2x2 intersection"
  unless (intersection square1x1 rect2x3 == square1x1) $ error "wrong 1x1 2x3 intersection"
  unless (isEmpty $ intersection square1x1 emptyArea1) $ error "wrong 1x1 empty 1 intersection"
  unless (isEmpty $ intersection square1x1 emptyArea2) $ error "wrong 1x1 empty 2 intersection"
  unless (isEmpty $ intersection square1x1 emptyArea3) $ error "wrong 1x1 empty 3 intersection"

  -- intersection is commutative
  unless (intersection square2x2 square1x1 == square1x1) $ error "wrong 1x1 2x2 intersection'"
  unless (intersection rect2x3 square1x1 == square1x1) $ error "wrong 1x1 2x3 intersection'"
  unless (isEmpty $ intersection emptyArea1 square1x1) $ error "wrong 1x1 empty 1 intersection'"
  unless (isEmpty $ intersection emptyArea2 square1x1) $ error "wrong 1x1 empty 2 intersection'"
  unless (isEmpty $ intersection emptyArea3 square1x1) $ error "wrong 1x1 empty 3 intersection'"
  return True
