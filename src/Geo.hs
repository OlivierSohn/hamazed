{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Geo ( Direction(..)
           , extend
           , Col(..)
           , Coords(..)
           , coordsForDirection
           , PosSpeed(..)
           , Segment(..)
           , bresenham
           , move
           , mkSegment
           , showSegment
           , changeSegmentLength
           , segmentContains
           , Row(..)
           , sumCoords
           , diffCoords
           , translate
           , translateInDir
           , zeroCoords
           , Vec2(..)
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
           ) where

import           Imajuscule.Prelude

import           GHC.Generics( Generic )

import           Util( takeWhileInclusive
                     , range )

--------------------------------------------------------------------------------
-- Pure
--------------------------------------------------------------------------------

data Direction = Up | Down | LEFT | RIGHT deriving (Eq, Show)

newtype Row = Row { _rowIndex :: Int } deriving (Generic, Eq, Show, Ord)
newtype Col = Col { _colIndex :: Int } deriving (Generic, Eq, Show, Ord)

data Coords = Coords {
    _x :: !Row
  , _y :: !Col
} deriving (Generic, Eq, Show, Ord)

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

coordsForDirection :: Direction -> Coords
coordsForDirection Down  = Coords (Row   1) (Col   0)
coordsForDirection Up    = Coords (Row$ -1) (Col   0)
coordsForDirection LEFT  = Coords (Row   0) (Col$ -1)
coordsForDirection RIGHT = Coords (Row   0) (Col   1)

multiply :: Int -> Coords -> Coords
multiply n (Coords (Row r) (Col c)) = Coords (Row $ r*n) (Col $ c*n)

translateInDir :: Direction -> Coords -> Coords
translateInDir dir = translate $ coordsForDirection dir


data Segment = Horizontal Row Int Int
             | Vertical   Col Int Int
             | Oblique    Coords Coords

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

-- returns Just (value - range start) if it is contained
rangeContains :: Int -> Int -> Int -> Maybe Int
rangeContains r1 r2 i = if abs (r2-i) + abs (i-r1) == abs (r2-r1) then Just (i - r1) else Nothing

data PosSpeed = PosSpeed {
    _pos :: !Coords
  , _speed :: !Coords
} deriving (Generic, Eq, Show, Ord)

rotateByQuarters :: Vec2 -> [Vec2]
rotateByQuarters v@(Vec2 x y) =
  [v,
  Vec2 x $ -y,
  Vec2 (-x) $ -y,
  Vec2 (-x) y]

data Vec2 = Vec2 Float Float deriving(Generic, Eq, Show)

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

bresenham :: Segment -> [Coords]
bresenham (Horizontal r c1 c2) = map (Coords r . Col) $ range c1 c2
bresenham (Vertical c r1 r2)   = map (flip Coords c . Row) $ range r1 r2
bresenham (Oblique (Coords (Row y0) (Col x0)) c2@(Coords (Row y1) (Col x1))) =
  takeWhileInclusive (/= c2) $ map (\(x,y) -> Coords (Row y) (Col x) ) $ bla (x0,y0) (x1,y1)

-- adapted from http://www.roguebasin.com/index.php?title=Bresenham%27s_Line_Algorithm#Haskell
balancedWord :: Int -> Int -> Int -> [Int]
balancedWord p q eps
  | eps + p < q = 0 : balancedWord p q (eps + p)
  | otherwise   = 1 : balancedWord p q (eps + p - q)

-- | Bresenham's line algorithm.
-- Includes the first point and goes through the second to infinity.
bla :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
bla (x0, y0) (x1, y1) =
  let (dx, dy) = (x1 - x0, y1 - y0)
      xyStep b (x, y) = (x + signum dx,     y + signum dy * b)
      yxStep b (x, y) = (x + signum dx * b, y + signum dy)
      (p, q, step) | abs dx > abs dy = (abs dy, abs dx, xyStep)
                   | otherwise       = (abs dx, abs dy, yxStep)
      walk w xy = xy : walk (tail w) (step (head w) xy)
  in  walk (balancedWord p q 0) (x0, y0)
