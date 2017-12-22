{-# LANGUAGE NoImplicitPrelude #-}

module Imj.UI.RectFrame
        (
          -- * RectFrame
          {- | 'RectFrame' Represents a UI rectangular frame of discrete dimensions.
          Wrapping it in a 'FrameAnimationParallel4' allows
          to transform it smoothly from one size / position to another. -}
          RectFrame(..)
        ) where

import           Imj.Prelude

import           Imj.Geo.Discrete.Types
import           Imj.Color.Types

data RectFrame = RectFrame {
    _rectFrameSize :: !Size
    -- ^ Width & Height
  , _rectFrameUpperLeft :: !Coords
    -- ^ Upper left corner
  , _rectFrameColors :: !LayeredColor
    -- ^ Foreground and background colors
} deriving(Eq, Show)
