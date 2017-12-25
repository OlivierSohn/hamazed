{-# LANGUAGE NoImplicitPrelude #-}

-- | This module exports animation functions.

module Imj.Animation.Geo
    (
    -- * Gravity
      gravityFallGeo
    -- * Explosion
    , simpleExplosionGeo
    , quantitativeExplosionGeo
    -- * Geometric figures
    , animatePolygonGeo
    -- * Laser
    , laserAnimationGeo
    ) where

import           Imj.Prelude

import           Data.Char( intToDigit )
import           Data.List( length )

import           Imj.Animation.Design.Internal.Types
import           Imj.Laser.Types
import           Imj.Geo.Continuous
import           Imj.Geo.Discrete
import           Imj.Iteration


-- | Note that the Coords parameter is unused.
laserAnimationGeo :: LaserRay Actual
                  -> Coords
                  -- ^ Unused, because the 'LaserRay' encodes the origin already
                  -> Frame
                  -> [AnimatedPoint]
laserAnimationGeo (LaserRay dir (Ray seg)) _ (Frame i) =
  let (originalChar, replacementChar) =
        if dir == LEFT || dir == RIGHT
          then
            ('=','-')
          else
            ('|','.')
      char = if i>= 2 then replacementChar else originalChar
      points = if i >= 4
                 then
                   []
                 else
                   bresenham seg
  in map (\p -> AnimatedPoint DontInteract p (Just char)) points

-- | Gravity free-fall
gravityFallGeo :: Vec2
               -- ^ Initial speed
               -> CanInteract
               -> Coords
               -- ^ Initial position
               -> Frame
               -> [AnimatedPoint]
gravityFallGeo initialSpeed canInteract origin (Frame iteration) =
  let o = pos2vec origin
      points = [vec2coords $ parabola o initialSpeed iteration]
  in map (\p -> AnimatedPoint canInteract p Nothing) points

-- | Circular explosion by copying quarter arcs.
simpleExplosionGeo :: Int
                   -- ^ Number of points per quarter arc.
                   -> CanInteract
                   -> Coords
                   -- ^ Center
                   -> Frame
                   -> [AnimatedPoint]
simpleExplosionGeo resolution canInteract center (Frame iteration) =
  let radius = fromIntegral iteration :: Float
      c = pos2vec center
      points = map vec2coords $ translatedFullCircleFromQuarterArc c radius 0 resolution
  in map (\p -> AnimatedPoint canInteract p Nothing) points

-- | Circular explosion
quantitativeExplosionGeo :: Int
                         -- ^ The number of points of the circle
                         -> CanInteract
                         -> Coords
                         -- ^ Center
                         -> Frame
                         -> [AnimatedPoint]
quantitativeExplosionGeo number canInteract center (Frame iteration) =
  let numRand = 10 :: Int
      rnd = 2 :: Int -- TODO store the random number in the state of the animation
  -- rnd <- getStdRandom $ randomR (0,numRand-1)
      radius = fromIntegral iteration :: Float
      firstAngle = (fromIntegral rnd :: Float) * 2*pi / (fromIntegral numRand :: Float)
      c = pos2vec center
      points = map vec2coords $ translatedFullCircle c radius firstAngle number
  in map (\p -> AnimatedPoint canInteract p Nothing) points

-- | Expanding then shrinking geometric figure.
animatePolygonGeo :: Int
                  -- ^ number of extremities of the polygon (if 1, draw a circle instead)
                  -> Coords
                  -- ^ Center
                  -> Frame
                  -- ^ Used to compute the radius.
                  -> [AnimatedPoint]
animatePolygonGeo n center (Frame i) =
  let r = animateRadius (quot i 2) n
      points = if r < 0
       then
         []
       else
         case n of
            1 -> let p = simpleExplosionGeo 8 DontInteract center $ Frame r
                 in map (\(AnimatedPoint _ p' _) -> p') p
            _ -> polygon n r center
  in map (\p -> AnimatedPoint DontInteract p (Just $ intToDigit n)) points

-- | A polygon using resampled bresenham to augment the number of points :
-- the number of points needs to be constant across the entire animation
-- so we need to resampleWithExtremities according to the biggest possible figure.
polygon :: Int -> Int -> Coords -> [Coords]
polygon nSides radius center =
  let startAngle = if odd nSides then pi else pi/4.0
      extrs = polyExtremities (pos2vec center) (fromIntegral radius) startAngle nSides
  in connect $ map vec2coords extrs

-- | Animate the radius by first expanding then shrinking.
animateRadius :: Int -> Int -> Int
animateRadius i nSides =
  let limit
          | nSides <= 4 = 5
          | nSides <= 6 = 7
          | otherwise   = 10
  in if i < limit
       then
         i
       else
         2 * limit - i

connect :: [Coords] -> [Coords]
connect []  = []
connect l@[_] = l
connect (a:rest@(b:_)) = connect2 a b ++ connect rest

connect2 :: Coords -> Coords -> [Coords]
connect2 start end =
  let numpoints = 80 -- more than 2 * (max height width of world) to avoid spaces
  in sampledBresenham numpoints start end

-- | Applies bresenham transformation and resamples it
sampledBresenham :: Int -> Coords -> Coords -> [Coords]
sampledBresenham nSamples start end =
  let l = bresenhamLength start end
      seg = mkSegment start end
      bres = bresenham seg
  in resampleWithExtremities bres (assert (l == length bres) l) nSamples
