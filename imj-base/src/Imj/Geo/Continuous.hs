{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Geo.Continuous
           (-- * Continuous coordinates
             Vec2(..)
           , module Imj.Geo.Continuous.Conversion
           -- * Sampled continuous geometry
           -- ** Circle
           , translatedFullCircle
           , translatedFullCircleFromQuarterArc
           -- ** Parabola
           , parabola
           -- * Polygon extremities
           , polyExtremities
           -- * Vec2 utilities
           , sumVec2d
           , scalarProd
           , rotateByQuarters
           -- * Reexports
           , Pos, Vel, Acc
           ) where

import           Imj.Prelude

import           Imj.Geo.Continuous.Types
import           Imj.Geo.Continuous.Conversion
import           Imj.Iteration

-- | Creates a list of 4 'Vec2' from a single one by rotating it successively by pi/2.
rotateByQuarters :: Vec2 Pos -> [Vec2 Pos]
rotateByQuarters v@(Vec2 x y) =
  [v,
  Vec2 x $ -y,
  Vec2 (-x) $ -y,
  Vec2 (-x) y]

-- | Sums two 'Vec2'.
{-# INLINE sumVec2d #-}
sumVec2d :: Vec2 a -> Vec2 a -> Vec2 a
sumVec2d (Vec2 vx vy) (Vec2 wx wy) = Vec2 (vx+wx) (vy+wy)

-- | Multiplies a 'Vec2' by a scalar.
scalarProd :: Float -> Vec2 a -> Vec2 a
scalarProd f (Vec2 x y) = Vec2 (f*x) (f*y)

-- | Integrate twice a constant acceleration over a duration, return a position
{-# INLINE integrateAcceleration2 #-}
integrateAcceleration2 :: Frame -> Vec2 Acc -> Vec2 Pos
integrateAcceleration2 (Frame time) (Vec2 vx vy) =
  let factor = 0.5 * fromIntegral (time * time)
  in Vec2 (vx * factor) (vy * factor)

-- | Integrate a constant velocity over a duration, return a position
{-# INLINE integrateVelocity #-}
integrateVelocity :: Frame -> Vec2 Vel -> Vec2 Pos
integrateVelocity (Frame time) (Vec2 vx vy) =
  let factor = fromIntegral time
  in Vec2 (vx * factor) (vy * factor)

gravity :: Vec2 Acc
gravity = Vec2 0 0.032 -- this number was adjusted so that the timing in Hamazed
                       -- game looks good. Instead, we could have adjusted the scale
                       -- of the world.

{-| Using
<https://en.wikipedia.org/wiki/Equations_of_motion equation [2] in "Constant linear acceleration in any direction">:

\[ \vec r = \vec r_0 + \vec v_0*t + {1 \over 2}* \vec a*t^2 \]

\[ where \]

\[ \vec r = current\;position \]

\[ \vec r_0 = initial\;position \]

\[ \vec v_0 = initial\;velocity \]

\[ \vec a = gravity\;force \]

\[ t = time \]

-}
parabola :: Vec2 Pos -> Vec2 Vel -> Frame -> Vec2 Pos
parabola r0 v0 time =
  let iv = integrateVelocity time v0
      ia = integrateAcceleration2 time gravity
  in sumVec2d r0 $ sumVec2d iv ia

mkPointOnCircle :: Float -> Float -> Vec2 Pos
mkPointOnCircle radius angle =
  let x = radius * sin angle
      y = radius * cos angle
  in Vec2 x y

discretizeArcOfCircle :: Float -> Float -> Float -> Int -> [Vec2 Pos]
discretizeArcOfCircle radius arcAngle firstAngle resolution =
  let angleIncrement = arcAngle / (fromIntegral resolution :: Float)
  in  map (\i ->
        let angle = firstAngle + angleIncrement * (fromIntegral i :: Float)
        in mkPointOnCircle radius angle) [0..resolution]

fullCircleFromQuarterArc :: Float -> Float -> Int -> [Vec2 Pos]
fullCircleFromQuarterArc radius firstAngle quarterArcResolution =
  let quarterArcAngle = pi/2
      quarterCircle = discretizeArcOfCircle radius quarterArcAngle firstAngle quarterArcResolution
  in  concatMap rotateByQuarters quarterCircle

fullCircle :: Float -> Float -> Int -> [Vec2 Pos]
fullCircle radius firstAngle resolution =
  let totalAngle = 2*pi
  in  discretizeArcOfCircle radius totalAngle firstAngle resolution

-- | Samples a circle in an optimized way, to reduce the number of 'sin' and 'cos'
-- calls.
--
-- The total number of points will always be a multiple of 4.
translatedFullCircleFromQuarterArc :: Vec2 Pos
                                   -- ^ Center
                                   -> Float
                                   -- ^ Radius
                                   -> Float
                                   -- ^ The angle corresponding to the first sampled point
                                   -> Int
                                   -- ^ The total number of sampled points __per quarter arc__.
                                   -> [Vec2 Pos]
translatedFullCircleFromQuarterArc center radius firstAngle resolution =
  let circle = fullCircleFromQuarterArc radius firstAngle resolution
  in map (sumVec2d center) circle

-- | Samples a circle.
translatedFullCircle :: Vec2 Pos
                     -- ^ Center
                     -> Float
                     -- ^ Radius
                     -> Float
                     -- ^ The angle corresponding to the first sampled point
                     -> Int
                     -- ^ The total number of sampled points
                     -> [Vec2 Pos]
translatedFullCircle center radius firstAngle resolution =
  let circle = fullCircle radius firstAngle resolution
  in map (sumVec2d center) circle

-- | Returns the extremities of a polygon. Note that it is equal to 'translatedFullCircle'
polyExtremities :: Vec2 Pos
                -- ^ Center
                -> Float
                -- ^ Radius
                -> Float
                -- ^ Rotation angle
                -> Int
                -- ^ Number of sides of the polygon.
                -> [Vec2 Pos]
polyExtremities = translatedFullCircle
