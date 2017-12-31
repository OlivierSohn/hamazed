{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.UI.Colored
           ( Colored(..)
           -- * Reexports
           , module Imj.Graphics.Class.Colorable
           ) where

import           Imj.Prelude

import           Imj.Graphics.Class.Colorable
import           Imj.Graphics.Class.DiscreteColorableMorphing
import           Imj.Graphics.Class.HasLayeredColor
import           Imj.Graphics.Color
import           Imj.Graphics.Interpolation


data Colored a = Colored {
    _coloredColor :: !LayeredColor
  , _coloredColorable :: !a
} deriving(Show)

instance HasLayeredColor (Colored a) where
  getColor (Colored color _) = color
  {-# INLINABLE getColor #-}

-- | 'Colored' can wrap a 'Colorable', to give it a notion of color.
instance (Colorable a)
      => Drawable (Colored a) where
  draw (Colored color colorable) =
    drawUsingColor colorable color

-- | Interpolates the color and morphs the 'Colorable' at the same time.
instance (DiscreteColorableMorphing a) => DiscreteDistance (Colored a) where
  distance (Colored colorFrom from) (Colored colorTo to) =
    max (distance from to) (distance colorFrom colorTo)
  {-# INLINABLE distance #-}

-- | 'Colored' can wrap a 'DiscreteColorableMorphing', to make a 'DiscreteMorphing'.
--
-- Interpolates the color and morphs the 'Colorable' at the same time.
instance (DiscreteColorableMorphing a) => DiscreteMorphing (Colored a) where
  drawMorphing (Colored colorFrom from) (Colored colorTo to) frame =
    drawMorphingUsingColor from to frame (interpolate colorFrom colorTo frame)
  {-# INLINABLE drawMorphing #-}