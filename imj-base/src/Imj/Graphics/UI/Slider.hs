{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Graphics.UI.Slider
        ( Slider(..)
        , SliderPresentation(..)
        ) where

import           Imj.Prelude

import           Data.Text(pack)
import qualified Data.Text as Text(length)
import           Imj.Geo.Discrete
import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.Color
import           Imj.Graphics.Font
import           Imj.Graphics.Render.FromMonadReader
import           Imj.Graphics.Text.ColoredGlyphList
import           Imj.Graphics.UI.Colored

{- |
    Horizontal linear slider, available in 2 rendering modes (see 'SliderPresentation')
-}
data Slider a = Slider {
    sliderValue, sliderMin, sliderMax :: !a
  , sliderStepCount :: {-# UNPACK #-} !Int
  , sliderDownChar, sliderUpChar :: {-# UNPACK #-} !Char
  , sliderColor :: {-# UNPACK #-} !LayeredColor
  , sliderPresentation :: !SliderPresentation
}

data SliderPresentation =
    Compact
    {- |
    @
                               6
          Block size : 'g' ....|...... 'y'

                                0.01
          Wall probability : 'h' |............... 'u'
    @
    -}
  | Large
  {- |
  @

                       6                 < numeric value (Int / Float)
        block size ....|......           < label, slider
                 'g' <   > 'y'           < Up / down chars

                        0.01
        wall probability |.................
                   'h' <   > 'u'

  @
  -}

{- | The coords passed to drawAt is the X position in the following examples:

'Large' mode:

@
      X   6
      ----|-------- block size
    'g' <   > 'y'
@

'Compact' mode:

@

      X                    6
      block size : 'g' ....|...... 'y'
@

For width in Positionable instance, we don't take increase / decrease keys
and label into account, to ease positionning.

NOTE We could define a notion of soft / hard dimensions : soft are total dimensions,
hard are dimensions used by alignment functions.
-}
instance (Real a, Show a) => Positionable (Slider a) where
  drawAt s@(Slider v _ _ nSteps up down color pres) coords = do
    drawGlyphs nSteps (gameGlyph barChar) barLeft color
    drawGlyph (gameGlyph '|') indicator color
    drawAligned_ num (mkCentered numCenter)
    case pres of
      Large ->
        drawAligned_ (Colored color upDown) (mkCentered upDownCenter)
      Compact -> do
        drawAt (Colored color compactLabel1) coords
        drawAt (Colored color compactLabel2) $ move nSteps RIGHT barLeft
   where
    i = getDiscreteIndex s
    barLeft = case pres of
      Large -> move 1 Down coords
      Compact -> move (Text.length compactLabel1) RIGHT coords
    indicator = move i RIGHT barLeft
    numCenter = move 1 Up indicator
    upDownCenter = move 1 Down indicator
    num = colored' (map gameGlyph $ show v) color
    upDown = case pres of
      Large -> '\'' : down : "' <   > '" ++ [up, '\'']
      Compact -> '\'' : down : "'/'" ++ [up, '\'']
    compactLabel1 = pack ['\'', down, '\'']
    compactLabel2 = pack ['\'', up, '\'']
    barChar = case pres of
      Large -> '-'
      Compact -> '.'
  width = fromIntegral . sliderStepCount

  height = (\case
    Large -> 3
    Compact -> 2) . sliderPresentation

  {-# INLINABLE width #-}
  {-# INLINABLE height #-}
  {-# INLINABLE drawAt #-}

{-# INLINABLE getDiscreteIndex #-}
getDiscreteIndex :: (Real a) => Slider a -> Int
getDiscreteIndex (Slider v min_ max_ nSteps _ _ _ _) = case delta of
  0 -> 0
  _ -> round $ normalizedValue * fromIntegral (nSteps - 1)
 where
  normalizedValue = realToFrac (v - min_) / delta
  delta :: Float
  delta = realToFrac $ max_ - min_
