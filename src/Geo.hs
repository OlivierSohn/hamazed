{-# LANGUAGE NoImplicitPrelude #-}

module Geo ( extend
           , rotateCcw
           , coordsForDirection
           , extremities
           , move
           , mkSegment
           , showSegment
           , changeSegmentLength
           , segmentContains
           , sumCoords
           , diffCoords
           , translate
           , translateInDir
           , zeroCoords
           , rotateByQuarters
           , vec2coords
           , pos2vec
           , speed2vec
           , sumVec2d
           , scalarProd
           -- circles
           , translatedFullCircle
           , translatedFullCircleFromQuarterArc
           -- curves
           , parabola
           -- poly extremities
           , polyExtremities
           -- | reexports
           , module Geo.Types
           ) where

import           Imajuscule.Prelude

import           Geo.Types

--------------------------------------------------------------------------------
-- Pure
--------------------------------------------------------------------------------

ccwDirections :: Int -> Direction
ccwDirections i = case i `mod` 4 of
                    0 -> Up
                    1 -> LEFT
                    2 -> Down
                    3 -> RIGHT
                    n -> error $ "out of bound modulo : " ++ show n

ccwDirectionsIndex :: Direction -> Int
ccwDirectionsIndex Up = 0
ccwDirectionsIndex LEFT = 1
ccwDirectionsIndex Down = 2
ccwDirectionsIndex RIGHT = 3

zeroCoords :: Coords
zeroCoords = Coords (Row 0) (Col 0)

sumCoords :: Coords -> Coords -> Coords
sumCoords (Coords (Row r1) (Col c1)) (Coords (Row r2) (Col c2)) = Coords (Row $ r1 + r2) (Col $ c1 + c2)

-- | a - b
diffCoords :: Coords
           -- ^ a
           -> Coords
           -- ^ b
           -> Coords
           -- ^ a - b
diffCoords (Coords (Row r1) (Col c1)) (Coords (Row r2) (Col c2)) = Coords (Row $ r1 - r2) (Col $ c1 - c2)

rotateCcw :: Int -> Direction -> Direction
rotateCcw n dir = ccwDirections $ n + ccwDirectionsIndex dir

coordsForDirection :: Direction -> Coords
coordsForDirection Down  = Coords (Row   1) (Col   0)
coordsForDirection Up    = Coords (Row$ -1) (Col   0)
coordsForDirection LEFT  = Coords (Row   0) (Col$ -1)
coordsForDirection RIGHT = Coords (Row   0) (Col   1)

multiply :: Int -> Coords -> Coords
multiply n (Coords (Row r) (Col c)) = Coords (Row $ r*n) (Col $ c*n)

translateInDir :: Direction -> Coords -> Coords
translateInDir dir = translate $ coordsForDirection dir


mkSegment :: Coords -> Coords -> Segment
mkSegment coord1@(Coords row@(Row r1) col@(Col c1)) coord2@(Coords (Row r2) (Col c2))
  | r1 == r2  = Horizontal row c1 c2
  | c1 == c2  = Vertical   col r1 r2
  | otherwise = Oblique coord1 coord2

showSegment :: Segment -> [Coords]
showSegment (Horizontal row c1 c2) = map (Coords row . Col) [(min c1 c2)..(max c1 c2)]
showSegment (Vertical col r1 r2)   = map (flip Coords col . Row) [(min r1 r2)..(max r1 r2)]
showSegment (Oblique _ _)          = error "oblique segment rendering is not supported"

changeSegmentLength :: Int -> Segment -> Segment
changeSegmentLength i (Horizontal row c1 _) = Horizontal row c1 $ c1 + i
changeSegmentLength i (Vertical   col r1 _) = Vertical col r1 $ r1 + i
changeSegmentLength _ _ = error "changeSegmentLength cannot operate on oblique segments"

-- returns the distance from segment start
segmentContains :: Coords -> Segment-> Maybe Int
segmentContains (Coords row' (Col c)) (Horizontal row c1 c2) = if row' == row then rangeContains c1 c2 c else Nothing
segmentContains (Coords (Row r) col') (Vertical   col r1 r2) = if col' == col then rangeContains r1 r2 r else Nothing
segmentContains _ _ = error "segmentContains cannot operate on oblique segments"

extremities :: Segment -> (Coords, Coords)
extremities (Horizontal row c1 c2) = (Coords row (Col c1), Coords row (Col c2))
extremities (Vertical   col r1 r2) = (Coords (Row r1) col, Coords (Row r2) col)
extremities (Oblique c1 c2)         = (c1, c2)

-- returns Just (value - range start) if it is contained
rangeContains :: Int -> Int -> Int -> Maybe Int
rangeContains r1 r2 i = if abs (r2-i) + abs (i-r1) == abs (r2-r1) then Just (i - r1) else Nothing

rotateByQuarters :: Vec2 -> [Vec2]
rotateByQuarters v@(Vec2 x y) =
  [v,
  Vec2 x $ -y,
  Vec2 (-x) $ -y,
  Vec2 (-x) y]

sumVec2d :: Vec2 -> Vec2 -> Vec2
sumVec2d (Vec2 vx vy) (Vec2 wx wy) = Vec2 (vx+wx) (vy+wy)

pos2vec :: Coords -> Vec2
pos2vec (Coords (Row r) (Col c)) = Vec2 (0.5 + fromIntegral c) (0.5 + fromIntegral r)

speed2vec :: Coords -> Vec2
speed2vec (Coords (Row r) (Col c)) = Vec2 (fromIntegral c) (fromIntegral r)

vec2coords :: Vec2 -> Coords
vec2coords (Vec2 x y) = Coords (Row $ floor y) (Col $ floor x)

scalarProd :: Float -> Vec2 -> Vec2
scalarProd f (Vec2 x y) = Vec2 (f*x) (f*y)

gravity :: Vec2
gravity = Vec2 0 0.2

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

-- Circle Functions ------------------------------------------------------------

mkPointOnCircle :: Float -> Float -> Vec2
mkPointOnCircle radius angle =
  let x = radius * sin angle
      y = radius * cos angle
  in Vec2 x y

polyExtremities :: Int -> Coords -> Int -> Float -> [Coords]
polyExtremities nSides center radius startAngle =
  map (sumCoords center . vec2coords) $ discretizeArcOfCircle (fromIntegral radius) (2.0*pi) startAngle nSides

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

translate :: Coords -> Coords -> Coords
translate = sumCoords

move :: Int -> Direction -> Coords -> Coords
move t dir c = sumCoords c $ multiply t $ coordsForDirection dir

extend :: Coords -> Direction -> (Coords -> Bool) -> Coords
extend coords dir continue =
  let loc = translateInDir dir coords
  in if continue loc
       then
         extend loc dir continue
       else
         coords
