{-# LANGUAGE NoImplicitPrelude #-}

module Geo.Discrete
           ( extend
           , coordsForDirection
           , extremities
           , move
           , mkSegment
           , showSegment
           , changeSegmentLength
           , segmentContains
           , diffCoords
           , translate
           , translate'
           , translateInDir
           , zeroCoords
           -- * reexports
           , module Geo.Discrete.Types
           ) where

import           Imajuscule.Prelude

import           Geo.Discrete.Types

zeroCoords :: Coords
zeroCoords = Coords 0 0

-- | a - b
diffCoords :: Coords
           -- ^ a
           -> Coords
           -- ^ b
           -> Coords
           -- ^ a - b
diffCoords (Coords r1 c1) (Coords r2 c2) = Coords (r1 - r2) (c1 - c2)


coordsForDirection :: Direction -> Coords
coordsForDirection Down  = Coords 1 0
coordsForDirection Up    = Coords (-1) 0
coordsForDirection LEFT  = Coords 0 (-1)
coordsForDirection RIGHT = Coords 0 1

multiply :: Int -> Coords -> Coords
multiply n (Coords r c) = Coords (r * fromIntegral n) (c * fromIntegral n)

translateInDir :: Direction -> Coords -> Coords
translateInDir dir = translate $ coordsForDirection dir


mkSegment :: Coords -> Coords -> Segment
mkSegment coord1@(Coords r1 c1) coord2@(Coords r2 c2)
  | r1 == r2  = Horizontal r1 c1 c2
  | c1 == c2  = Vertical   c1 r1 r2
  | otherwise = Oblique coord1 coord2

showSegment :: Segment -> [Coords]
showSegment (Horizontal row c1 c2) = map (Coords row) [(min c1 c2)..(max c1 c2)]
showSegment (Vertical col r1 r2)   = map (flip Coords col) [(min r1 r2)..(max r1 r2)]
showSegment (Oblique _ _)          = error "oblique segment rendering is not supported"

changeSegmentLength :: Int -> Segment -> Segment
changeSegmentLength i (Horizontal row c1 _) = Horizontal row c1 $ c1 + fromIntegral i
changeSegmentLength i (Vertical   col r1 _) = Vertical   col r1 $ r1 + fromIntegral i
changeSegmentLength _ _ = error "changeSegmentLength cannot operate on oblique segments"

-- returns the distance from segment start
segmentContains :: Coords -> Segment-> Maybe Int
segmentContains (Coords row' c') (Horizontal row c1' c2') = if row' == row then rangeContains c1 c2 c else Nothing
 where
   c = fromIntegral c'
   c1 = fromIntegral c1'
   c2 = fromIntegral c2'
segmentContains (Coords r' col') (Vertical   col r1' r2') = if col' == col then rangeContains r1 r2 r else Nothing
 where
   r = fromIntegral r'
   r1 = fromIntegral r1'
   r2 = fromIntegral r2'
segmentContains _ _ = error "segmentContains cannot operate on oblique segments"

extremities :: Segment -> (Coords, Coords)
extremities (Horizontal row c1 c2) = (Coords row c1, Coords row c2)
extremities (Vertical   col r1 r2) = (Coords r1 col, Coords r2 col)
extremities (Oblique c1 c2)         = (c1, c2)

-- returns Just (value - range start) if it is contained
rangeContains :: Int -> Int -> Int -> Maybe Int
rangeContains r1 r2 i = if abs (r2-i) + abs (i-r1) == abs (r2-r1) then Just (i - r1) else Nothing

translate :: Coords -> Coords -> Coords
translate = sumCoords

translate' :: Coord Row -> Coord Col -> Coords -> Coords
translate' r c = sumCoords (Coords r c)

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
