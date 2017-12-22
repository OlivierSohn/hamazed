{-# LANGUAGE NoImplicitPrelude #-}

-- | This module handles colors for animations

module Imj.Animation.Color
        ( colorFromFrame
        , module Imj.Color
        ) where

import           Imj.Prelude

import           Imj.Animation.Types
import           Imj.Color

-- | Returns the color associated to an animation frame.
colorFromFrame :: Frame -> LayeredColor
colorFromFrame (Frame f) = onBlack $ rgb r g b
  where
    r = assert (f >= 0) 4
    g = fromIntegral $ (0 + quot f 6) `mod` 2 -- [0..1] , slow changes
    b = fromIntegral $ (0 + quot f 3) `mod` 3 -- [0..2] , 2x faster changes
