{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Imj.Graphics.UI.Colored
           ( Colored(..)
           -- * Reexports
           , module Imj.Graphics.Class.Colorable
           ) where

import           Imj.Prelude

import           Imj.Graphics.Class.Colorable
import           Imj.Graphics.Class.DiscreteColorableMorphing
import           Imj.Graphics.Class.HasLayeredColor
import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.Class.UncoloredTextual
import           Imj.Graphics.Interpolation


data Colored a = Colored {
    _coloredColor :: {-# UNPACK #-} !LayeredColor
  , _coloredColorable :: !a
} deriving(Generic, Show, PrettyVal)
instance Functor Colored where
  fmap f (Colored color a) = Colored color $ f a
instance (HasReferencePosition a) => HasReferencePosition (Colored a) where
  getPosition (Colored _ a) = getPosition a
  {-# INLINE getPosition #-}
instance (GeoTransform a) => GeoTransform (Colored a) where
  transform f = fmap (transform f)
  {-# INLINE transform #-}
  
instance (UncoloredTextual t) => Positionable (Colored t) where
  drawAt (Colored color txt) pos = drawTextual txt pos color
  width (Colored _ txt) = textLength txt
  height _ = 1
  {-# INLINABLE drawAt #-}
  {-# INLINABLE width #-}
  {-# INLINABLE height #-}

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
