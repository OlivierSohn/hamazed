{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.ParticleSystem.Design.Color
        ( colorFromFrame
        ) where

import           Imj.Prelude

import           Imj.Graphics.Color
import           Imj.Graphics.Color.Types
import           Imj.Iteration

-- | Returns the color used to draw a 'Particle', based on the frame number
-- and a /base/ 'Color8'.
colorFromFrame :: Color8 Foreground
               -- ^ Base color, expected to be an rgb with r <= 6, g <= 5, b <= 4.
               -> Frame
               -> Color8 Foreground
colorFromFrame fgBase (Frame f) =
  case color8CodeToXterm256 fgBase of
    RGBColor (RGB r' g' b') -> assert (f >= 0) $ rgb r g b
      where
        r = r'
        g = fromIntegral $ (fromIntegral g' + quot f 6) `mod` 2 -- [0..1] , slow changes
        b = fromIntegral $ (fromIntegral b' + quot f 3) `mod` 3 -- [0..2] , 2x faster changes
    _ -> error "should be an rgb"
