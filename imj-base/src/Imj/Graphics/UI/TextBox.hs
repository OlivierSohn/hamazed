{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}

module Imj.Graphics.UI.TextBox
        ( TextBox(..)
        , mkTextBox
        , HeightAdjustPolicy(..)
        , BackgroundColorPolicy(..)
        , mkAdjustableHeightTextBox
        , addText
        ) where

import           Imj.Prelude
import           Prelude(length)

import           Imj.Geo.Discrete
import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.Class.Words
import           Imj.Graphics.Color
import           Imj.Graphics.Font
import           Imj.Graphics.Render.FromMonadReader
import           Imj.Graphics.Text.ColorString hiding(take)
import           Imj.Graphics.UI.RectArea
import           Imj.Graphics.UI.RectContainer

-- | The invariant is that no 'Line' should be wider than the width of 'Size'
data TextBox a = TextBox {
    getBox :: {-# UNPACK #-} !Size
  , getLines :: [[a]]
  -- ^ From bottom to top.
  , adjustment :: {-unpack sum-} !HeightAdjustPolicy
  , getBgColorPolicy :: {-unpack sum-} !BackgroundColorPolicy
}
instance Positionable (TextBox ColorString) where
  drawAt (TextBox sizeContent@(Size h w) strs _ color) coords = do
    let enclosingContainer = RectContainer sizeContent coords
    drawUsingColor enclosingContainer (LayeredColor black (gray 5))
    let upperLeftContent = translate coords $ Coords 1 1
        lowerLeftContent = translate coords $ Coords (fromIntegral h) 1
        contentArea = mkRectArea upperLeftContent sizeContent
        space = textGlyph ' '
    usingScissor contentArea $ fill space whiteOnBlack
    foldM_
      (\lineStart (c, str) -> do
        let nLines = Prelude.length str
            upperLeftGroup = move (pred nLines) Up lineStart
            szGroup = Size (fromIntegral nLines) w
        usingScissor (intersection contentArea $ mkRectArea upperLeftGroup szGroup) $ do
          modStr <- case c of
            NoColor -> return id
            Alternate False -> return id
            Alternate True -> do
              let bg = gray 0
              fill space $ LayeredColor bg white
              return $ replaceBackground bg
          foldM
            (\pos s-> do
                drawAt s pos
                return $ translateInDir Up pos)
            lineStart
            $ map modStr str)
      lowerLeftContent
      $ zip (iterate alternate color) strs

  -- | Takes enclosing frame into account
  width (TextBox (Size _ w) _ _ _)  = 2 + fromIntegral w
  height (TextBox (Size h _) _ _ _) = 2 + fromIntegral h
  {-# INLINABLE width #-}
  {-# INLINABLE height #-}
  {-# INLINABLE drawAt #-}

data BackgroundColorPolicy =
    NoColor
  | Alternate !Bool

alternate :: BackgroundColorPolicy -> BackgroundColorPolicy
alternate NoColor = NoColor
alternate (Alternate v) = Alternate $ not v

data HeightAdjustPolicy =
    NoAdjustment
  | Adjust

mkAdjustableHeightTextBox :: Length Width -> BackgroundColorPolicy -> TextBox a
mkAdjustableHeightTextBox w = TextBox (Size 0 w) [] Adjust

mkTextBox :: Size -> BackgroundColorPolicy -> TextBox a
mkTextBox s = TextBox s [] NoAdjustment

{-# INLINABLE addText #-}
addText :: (Words t)
        => t
        -> TextBox t
        -> TextBox t
addText t (TextBox s@(Size h w) l adjustHeight color) =
  let tLines = multiLine (fromIntegral w) t
      newLines = reverse tLines : l
      tb = case adjustHeight of
        NoAdjustment ->
          -- given that each group of lines has at least one line, if we take 'h' groups,
          -- we have enought to fill the box.
          TextBox s $ take (fromIntegral h) newLines
        Adjust ->
          TextBox (Size newH w) newLines
          where newH = fromIntegral $ Prelude.length newLines
  in tb adjustHeight $ alternate color
