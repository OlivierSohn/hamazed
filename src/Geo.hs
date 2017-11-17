{-# LANGUAGE DeriveGeneric #-}

module Geo ( Direction(..)
           , Col(..)
           , Coords(..)
           , coordsForDirection
           , PosSpeed(..)
           , Segment(..)
           , mkSegment
           , changeSegmentLength
           , segmentContains
           , rotateByQuarters
           , Row(..)
           , sumCoords
           , translate
           , translateInDir
           , translateLeft
           , zeroCoords
           -- circles
           , translatedFullCircle
           , translatedFullCircleFromQuarterArc
           ) where

import           Imajuscule.Prelude

import           GHC.Generics( Generic )

--------------------------------------------------------------------------------
-- Pure
--------------------------------------------------------------------------------

data Direction = Up | Down | LEFT | RIGHT deriving (Eq, Show)

newtype Row = Row { _rowIndex :: Int } deriving (Generic, Eq, Show)
newtype Col = Col { _colIndex :: Int } deriving (Generic, Eq, Show)

data Coords = Coords {
    _x :: !Row
  , _y :: !Col
} deriving (Generic, Eq, Show)


zeroCoords :: Coords
zeroCoords = Coords (Row 0) (Col 0)


sumCoords :: Coords -> Coords -> Coords
sumCoords (Coords (Row r1) (Col c1)) (Coords (Row r2) (Col c2)) = Coords (Row $ r1 + r2) (Col $ c1 + c2)


coordsForDirection :: Direction -> Coords
coordsForDirection Down  = Coords (Row   1) (Col   0)
coordsForDirection Up    = Coords (Row$ -1) (Col   0)
coordsForDirection LEFT  = Coords (Row   0) (Col$ -1)
coordsForDirection RIGHT = Coords (Row   0) (Col   1)


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
}

rotateByQuarters :: Coords -> [Coords]
rotateByQuarters co@(Coords (Row r) (Col c)) =
  [co,
  Coords (Row c) (Col $ -r),
  Coords (Row $ -c) (Col r),
  Coords (Row $ -r) (Col $ -c)]


-- Circle Functions ------------------------------------------------------------

mkPointOnCircle :: Float -> Float -> Coords
mkPointOnCircle radius angle =
  let x = radius * sin angle
      y = radius * cos angle
      toInt flt = floor $ 0.5 + flt
  in Coords (Row $ toInt y) (Col $ toInt x)

discretizeArcOfCircle :: Float -> Float -> Float -> Int -> [Coords]
discretizeArcOfCircle radius arcAngle firstAngle resolution =
  let angleIncrement = arcAngle / (fromIntegral resolution :: Float)
  in  map (\i ->
        let angle = firstAngle + angleIncrement * (fromIntegral i :: Float)
        in mkPointOnCircle radius angle) [0..resolution]

fullCircleFromQuarterArc :: Float -> Float -> Int -> [Coords]
fullCircleFromQuarterArc radius firstAngle quarterArcResolution =
  let quarterArcAngle = pi/2
      quarterCircle = discretizeArcOfCircle radius quarterArcAngle firstAngle quarterArcResolution
  in  concatMap rotateByQuarters quarterCircle

fullCircle :: Float -> Float -> Int -> [Coords]
fullCircle radius firstAngle resolution =
  let totalAngle = 2*pi
  in  discretizeArcOfCircle radius totalAngle firstAngle resolution

translatedFullCircleFromQuarterArc :: Coords -> Float -> Float -> Int -> [Coords]
translatedFullCircleFromQuarterArc center radius firstAngle resolution =
  let circle = fullCircleFromQuarterArc radius firstAngle resolution
  in map (translate center) circle

translatedFullCircle :: Coords -> Float -> Float -> Int -> [Coords]
translatedFullCircle center radius firstAngle resolution =
  let circle = fullCircle radius firstAngle resolution
  in map (translate center) circle

translate :: Coords -> Coords -> Coords
translate = sumCoords

translateLeft :: Int -> Coords -> Coords
translateLeft t (Coords (Row r) (Col c)) = Coords (Row r) (Col (c-t))
