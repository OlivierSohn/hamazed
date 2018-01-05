{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}

module Imj.Graphics.UI.RectArea
  ( RectArea(..)
  , mkRectArea
  , growRectArea
  , maxRectArea
  , reactAreaSize
  , contains
  , intersection
  , isEmpty
  , rectAreaCenter
  , Filter
  , Positive
  , Negative
  -- * reexport
  , Size
  ) where

import           Imj.Prelude

import           Imj.Geo.Discrete


-- | Defines a 2d rectangular area, with inclusive bounds.
--
-- If one coordinate of upper left is bigger than its respective lower right coordinate,
-- the area is interpreted as empty.
--
-- we use phantom type 'Filter' @x@ to tell how the are should be interpreted.
data RectArea a = RectArea {
    _viewportInclusiveUpperLeft :: {-# UNPACK #-} !(Coords Pos)
  , _viewportInclusiveLowerRight :: {-# UNPACK #-} !(Coords Pos)
} deriving(Show)

-- | with phantom types 'Positive' and 'Negative' to tell the kind of filter.
data Filter a

-- | If 'Coords' are outside 'Area', they will not be selected.
--
-- it is a scissor in the sense of
-- <https://www.khronos.org/registry/OpenGL-Refpages/es2.0/xhtml/glScissor.xml glScissor>
data Positive

-- | The opposite of a 'Positive' : if 'Coords' are inside 'Area', they will not be selected.
data Negative

maxRectArea :: RectArea a
maxRectArea = RectArea (Coords minBound minBound) (Coords maxBound maxBound)

{-# INLINE isEmpty #-}
isEmpty :: RectArea a -> Bool
isEmpty (RectArea (Coords r1 c1) (Coords r2 c2)) =
  r1 > r2 || c1 > c2

{-# INLINE contains #-}
contains :: RectArea a -> Coords Pos -> Bool
contains a@(RectArea (Coords r1 c1) (Coords r2 c2)) (Coords r c)
  -- viewport could be empty:
  | isEmpty a = False
  -- coordinates could be outside bounds:
  | otherwise = not $ r < r1 || r > r2 || c < c1 || c > c2

rectAreaCenter :: RectArea a -> Coords Pos
rectAreaCenter (RectArea from to) =
  let (Coords h w) = diffCoords to from
  in translate from $ Coords (quot h 2) (quot w 2)

mkRectArea :: Coords Pos -> Size -> RectArea a
mkRectArea upperLeft (Size h w) =
  let lowerRight = translate' (fromIntegral $ pred h) (fromIntegral $ pred w) upperLeft
  in RectArea upperLeft lowerRight

growRectArea :: Int -> RectArea a -> RectArea a
growRectArea i' (RectArea from to) =
  RectArea (translate' (-i) (fromIntegral $ -i) from) (translate' i (fromIntegral i) to)
 where
  !i = fromIntegral i'

reactAreaSize :: RectArea a -> Size
reactAreaSize r@(RectArea (Coords r1 c1) (Coords r2 c2)) =
  if isEmpty r
    then
      Size 0 0
    else
      let h = 1 + (r2 - r1)
          w = 1 + (c2 - c1)
      in Size (fromIntegral h) (fromIntegral w)

intersection :: RectArea a -> RectArea a -> RectArea a -- TODO unit test to verify it alway works
-- (irrespective of how the bounds are swapped, and even if both are empty)
intersection (RectArea (Coords r1 c1) (Coords r2 c2))
             (RectArea (Coords r1' c1') (Coords r2' c2')) =
  RectArea (Coords (max r1 r1') (max c1 c1')) (Coords (min r2 r2') (min c2 c2'))
