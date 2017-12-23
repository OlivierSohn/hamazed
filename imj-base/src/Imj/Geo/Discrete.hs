{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Geo.Discrete
           ( module Imj.Geo.Discrete.Types
           -- * Construct Segment
           , mkSegment
           , mkSegmentByExtendingWhile
           , changeSegmentLength
           -- * Use Segment
           , extremities
           , segmentContains
           -- * Construct Coords
           , zeroCoords
           , coordsForDirection
           -- * Use Coords
           , diffCoords
           , sumCoords
           , move
           , translate
           , translate'
           , translateInDir
           -- * Bresenham algorithms
           {- | The 2d version, 'bresenham', allows to draw a line on a 2d grid.

           The 3d version, 'bresenham3', allows to interpolate discrete colors in RGB space.
           -}
           , module Imj.Geo.Discrete.Bresenham
           , module Imj.Geo.Discrete.Bresenham3
           -- * Resampling
           , module Imj.Geo.Discrete.Resample
          ) where

import           Imj.Prelude

import           Imj.Geo.Discrete.Types
import           Imj.Geo.Discrete.Bresenham
import           Imj.Geo.Discrete.Bresenham3
import           Imj.Geo.Discrete.Resample

-- | 'zeroCoords' = 'Coords' 0 0
zeroCoords :: Coords
zeroCoords = Coords 0 0

-- | Returns a - b
diffCoords :: Coords
           -- ^ a
           -> Coords
           -- ^ b
           -> Coords
           -- ^ a - b
diffCoords (Coords r1 c1) (Coords r2 c2) =
  Coords (r1 - r2) (c1 - c2)

-- | Returns a + b
sumCoords :: Coords
           -- ^ a
           -> Coords
           -- ^ b
           -> Coords
           -- ^ a + b
sumCoords (Coords r1 c1) (Coords r2 c2) =
  Coords (r1 + r2) (c1 + c2)

-- | Returns the coordinates that correspond to one step in the given direction.
coordsForDirection :: Direction -> Coords
coordsForDirection Down  = Coords 1 0
coordsForDirection Up    = Coords (-1) 0
coordsForDirection LEFT  = Coords 0 (-1)
coordsForDirection RIGHT = Coords 0 1

multiply :: Int -> Coords -> Coords
multiply n (Coords r c) = Coords (r * fromIntegral n) (c * fromIntegral n)

-- | Translate of 1 step in a given direction.
translateInDir :: Direction -> Coords -> Coords
translateInDir dir = translate $ coordsForDirection dir


mkSegment :: Coords
          -- ^ Segment start
          -> Coords
          -- ^ Segment end
          -> Segment
mkSegment coord1@(Coords r1 c1) coord2@(Coords r2 c2)
  | r1 == r2  = Horizontal r1 c1 c2
  | c1 == c2  = Vertical   c1 r1 r2
  | otherwise = Oblique coord1 coord2


-- | Modify the end of the segment to reach the given length
changeSegmentLength :: Int -> Segment -> Segment
changeSegmentLength i (Horizontal row c1 _) = Horizontal row c1 $ c1 + fromIntegral i
changeSegmentLength i (Vertical   col r1 _) = Vertical   col r1 $ r1 + fromIntegral i
changeSegmentLength _ _ = error "changeSegmentLength cannot operate on oblique segments" -- TODO use bresenham if it is valid

-- | Returns the distance from segment start
segmentContains :: Coords
                -- ^ The coordinates to test
                -> Segment
                -> Maybe Int
                -- ^ 'Nothing' if the coordinate is not contained, else 'Just'
                -- the distance from segment start.
segmentContains (Coords row' c) (Horizontal row c1 c2) =
  if row' == row
    then
      fromIntegral <$> rangeContains c1 c2 c
    else
      Nothing
segmentContains (Coords r col') (Vertical col r1 r2) =
  if col' == col
    then
      fromIntegral <$> rangeContains r1 r2 r
    else
      Nothing
segmentContains _ _ =
  error "segmentContains cannot operate on oblique segments" -- TODO use bresenham

-- | Returns the start and end coordinates.
extremities :: Segment -> (Coords, Coords)
extremities (Horizontal row c1 c2) = (Coords row c1, Coords row c2)
extremities (Vertical   col r1 r2) = (Coords r1 col, Coords r2 col)
extremities (Oblique c1 c2)         = (c1, c2)

-- returns Just (value - range start) if it is contained
{-# INLINABLE rangeContains #-}
rangeContains :: (Num a, Eq a) => a -> a -> a -> Maybe a
rangeContains r1 r2 i =
  if abs (r2-i) + abs (i-r1) == abs (r2-r1)
    then
      Just (i - r1)
    else
      Nothing

-- | 'translate' = 'sumCoords'
translate :: Coords -> Coords -> Coords
translate = sumCoords

-- | Translate by a given height and width.
translate' :: Length Height
           -- ^ The height to add
           -> Length Width
           -- ^ The width to add
           -> Coords
           -- The initial coordinates
           -> Coords
translate' h w c =
  sumCoords c $ toCoords h w

move :: Int
     -- ^ Take that many steps
     -> Direction
     -- ^ In that direction
     -> Coords
     -- ^ From these coordinates
     -> Coords
move t dir c = sumCoords c $ multiply t $ coordsForDirection dir

mkSegmentByExtendingWhile :: Coords
                          -- ^ start of the segment
                          -> Direction
                          -- ^ 'Direction' in which to extend
                          -> (Coords -> Bool)
                          -- ^ Continue extension while this functions returns 'True'.
                          -> Segment
mkSegmentByExtendingWhile start dir f =
  let end = extend' start dir f
  in mkSegment start end

extend' :: Coords -> Direction -> (Coords -> Bool) -> Coords
extend' coords dir continue =
  let loc = translateInDir dir coords
  in if continue loc
       then
         extend' loc dir continue
       else
         coords
