{-# LANGUAGE NoImplicitPrelude #-}

-- | Helper functions for pure animation functions

module Imj.Animation.Geo
    (
    -- * Gravity
      gravityFall
    -- * Explosion
    , simpleExplosionPure
    , quantitativeExplosionPure
    -- * Geometric figures
    , animatePolygonPure
    -- * Laser
    , laserAnimationPure
    ) where

import           Imj.Prelude

import           Data.Char( intToDigit )
import           Data.List( length )

import           Imj.Laser.Types
import           Imj.Geo.Continuous
import           Imj.Geo.Discrete
import           Imj.Iteration


-- | Note that the Coords parameter is unused.
laserAnimationPure :: LaserRay Actual
                   -> Coords
                   -- ^ Unused, because the 'LaserRay' encodes the origin already
                   -> Frame
                   -> ([Coords], Maybe Char)
laserAnimationPure (LaserRay dir (Ray seg)) _ (Frame i) =
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
  in (points, Just char)

-- | Gravity free-fall
gravityFall :: Vec2
            -- ^ Initial speed
            -> Coords
            -- ^ Initial position
            -> Frame
            -> ([Coords], Maybe Char)
gravityFall initialSpeed origin (Frame iteration) =
  let o = pos2vec origin
  in  ([vec2coords $ parabola o initialSpeed iteration], Nothing)

-- | Circular explosion by copying quarter arcs.
simpleExplosionPure :: Int
                    -- ^ Number of points per quarter arc.
                    -> Coords
                    -- ^ Center
                    -> Frame
                    -> ([Coords], Maybe Char)
simpleExplosionPure resolution center (Frame iteration) =
  let radius = fromIntegral iteration :: Float
      c = pos2vec center
  in (map vec2coords $ translatedFullCircleFromQuarterArc c radius 0 resolution, Nothing)

-- | Circular explosion
quantitativeExplosionPure :: Int
                          -- ^ The number of points of the circle
                          -> Coords
                          -- ^ Center
                          -> Frame
                          -> ([Coords], Maybe Char)
quantitativeExplosionPure number center (Frame iteration) =
  let numRand = 10 :: Int
      rnd = 2 :: Int -- TODO store the random number in the state of the animation
  -- rnd <- getStdRandom $ randomR (0,numRand-1)
      radius = fromIntegral iteration :: Float
      firstAngle = (fromIntegral rnd :: Float) * 2*pi / (fromIntegral numRand :: Float)
      c = pos2vec center
  in (map vec2coords $ translatedFullCircle c radius firstAngle number, Nothing)

-- | Expanding then shrinking geometric figure.
animatePolygonPure :: Int
                   -- ^ number of extremities of the polygon (if 1, draw a circle instead)
                   -> Coords
                   -- ^ Center
                   -> Frame
                   -- ^ Used to compute the radius.
                   -> ([Coords], Maybe Char)
animatePolygonPure n center (Frame i) =
  let r = animateRadius (quot i 2) n
      points = if r < 0
       then
         []
       else
         case n of
            1 -> fst $ simpleExplosionPure 8 center $ Frame r
            _ -> polygon n r center
  in (points, Just $ intToDigit n)

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
