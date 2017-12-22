{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Geo.Continuous
           ( rotateByQuarters
           , sumVec2d
           , scalarProd
           -- * Circles
           , translatedFullCircle
           , translatedFullCircleFromQuarterArc
           -- * Curves
           , parabola
           -- * Poly extremities
           , polyExtremities
           -- * Reexports
           , module Imj.Geo.Continuous.Types
           ) where

import           Imj.Prelude


import           Imj.Geo.Continuous.Types


rotateByQuarters :: Vec2 -> [Vec2]
rotateByQuarters v@(Vec2 x y) =
  [v,
  Vec2 x $ -y,
  Vec2 (-x) $ -y,
  Vec2 (-x) y]

sumVec2d :: Vec2 -> Vec2 -> Vec2
sumVec2d (Vec2 vx vy) (Vec2 wx wy) = Vec2 (vx+wx) (vy+wy)

scalarProd :: Float -> Vec2 -> Vec2
scalarProd f (Vec2 x y) = Vec2 (f*x) (f*y)

gravity :: Vec2
gravity = Vec2 0 0.2 -- this number was adjusted so that the timing in Hamazed
                     -- game looks good. Instead, we could have adjusted the scale
                     -- of the world.

-- using https://en.wikipedia.org/wiki/Equations_of_motion :
-- equation [2] in "Constant linear acceleration in any direction"
--   r = r0 + v0t + .5*at^2
-- where
--   a = gravity force
--   t = time
--   r0 = initial position
--   v0 = initial velocity
parabola :: Vec2 -> Vec2 -> Int -> Vec2
parabola r0 v0 time =
  let t = 0.4 * fromIntegral time
  in sumVec2d (scalarProd (0.5*t*t) gravity) (sumVec2d r0 (scalarProd t v0))

mkPointOnCircle :: Float -> Float -> Vec2
mkPointOnCircle radius angle =
  let x = radius * sin angle
      y = radius * cos angle
  in Vec2 x y

discretizeArcOfCircle :: Float -> Float -> Float -> Int -> [Vec2]
discretizeArcOfCircle radius arcAngle firstAngle resolution =
  let angleIncrement = arcAngle / (fromIntegral resolution :: Float)
  in  map (\i ->
        let angle = firstAngle + angleIncrement * (fromIntegral i :: Float)
        in mkPointOnCircle radius angle) [0..resolution]

fullCircleFromQuarterArc :: Float -> Float -> Int -> [Vec2]
fullCircleFromQuarterArc radius firstAngle quarterArcResolution =
  let quarterArcAngle = pi/2
      quarterCircle = discretizeArcOfCircle radius quarterArcAngle firstAngle quarterArcResolution
  in  concatMap rotateByQuarters quarterCircle

fullCircle :: Float -> Float -> Int -> [Vec2]
fullCircle radius firstAngle resolution =
  let totalAngle = 2*pi
  in  discretizeArcOfCircle radius totalAngle firstAngle resolution

translatedFullCircleFromQuarterArc :: Vec2 -> Float -> Float -> Int -> [Vec2]
translatedFullCircleFromQuarterArc center radius firstAngle resolution =
  let circle = fullCircleFromQuarterArc radius firstAngle resolution
  in map (sumVec2d center) circle

translatedFullCircle :: Vec2 -> Float -> Float -> Int -> [Vec2]
translatedFullCircle center radius firstAngle resolution =
  let circle = fullCircle radius firstAngle resolution
  in map (sumVec2d center) circle

polyExtremities :: Int -> Vec2 -> Int -> Float -> [Vec2]
polyExtremities nSides center radius startAngle =
  map (sumVec2d center) $ discretizeArcOfCircle (fromIntegral radius) (2.0*pi) startAngle nSides
