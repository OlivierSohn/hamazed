{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Imj.Graphics.UI.RectContainer
        ( RectContainer(..)
        , translateRectContainer
        , mkRectContainerWithTotalArea
        , mkRectContainerAtDistance
        , mkRectContainerWithCenterAndInnerSize
        , upperLeftFromCenterAndSize
        , getSideCenters
          -- * Reexports
        , Colorable(..)
        ) where

import           Imj.Prelude

import           Data.List( mapAccumL, zip )
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Class.DiscreteColorableMorphing
import           Imj.Graphics.Class.HasRectArea
import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.UI.RectArea
import           Imj.Graphics.UI.RectContainer.MorphParallel4

{-|

@
r----------------------------+
| u--+                       |
| |//|                       |
| |//|                       |
| +--l                       |
|                            |
+----------------------------+

r = Terminal origin, at (0,0)
/ = RectContainer's content, of size (2,2)
u = RectContainer's upper left corner, at (2,1)
l = RectContainer's lower left corner, at (5,4)
@
-}
data RectContainer = RectContainer {
    _rectFrameContentSize :: {-# UNPACK #-} !Size
    -- ^ /Content/ size.
  , _rectFrameUpperLeft :: {-# UNPACK #-} !(Coords Pos)
    -- ^ Upper left corner.
} deriving(Eq, Show, Generic, PrettyVal)
instance HasReferencePosition RectContainer where
  getPosition (RectContainer _ ul) = ul
  {-# INLINE getPosition #-}
instance GeoTransform RectContainer where
  transform f (RectContainer sz ul) = RectContainer sz (transform f ul)
  {-# INLINE transform #-}

instance Colorable RectContainer where
  drawUsingColor = drawWhole
  {-# INLINABLE drawUsingColor #-}

-- | Returns the content area, /excluding/ the frame around it.
instance HasRectArea RectContainer where
  getRectArea (RectContainer (Size h w) upperLeft) =
    RectArea (sumCoords upperLeft $ toCoords 1 1) (translate upperLeft $ toCoords h w)
  {-# INLINABLE getRectArea #-}

-- | Smoothly transforms the 4 sides of the rectangle simultaneously, from their middle
-- to their extremities.
instance DiscreteDistance RectContainer where
  {-# INLINABLE distance #-}
  distance c@(RectContainer s  _)
           c'@(RectContainer s' _)
    | c == c'   = 1
    | otherwise = 1 + quot (1 + max (maxLength s) (maxLength s')) 2

instance DiscreteColorableMorphing RectContainer where
  {-# INLINABLE drawMorphingUsingColor #-}
  drawMorphingUsingColor from to frame color
    | frame <= 0         = drawUsingColor from color
    | frame >= lastFrame = drawUsingColor to color
    | otherwise          = drawRectFrameInterpolation from to lastFrame frame color
    where
      lastFrame = distance from to - 1

-- TODO typeclass "continuous closed path" to gather 'ranges' and 'drawRectFrameInterpolation' logics.


translateRectContainer :: Coords Pos -> RectContainer -> RectContainer
translateRectContainer offset (RectContainer a coords) =
  RectContainer a $ sumCoords offset coords

{-# INLINABLE drawWhole #-}
drawWhole :: (Draw e, MonadReader e m, MonadIO m)
          => RectContainer
          -> LayeredColor
          -> m ()
drawWhole (RectContainer sz upperLeft) =
  drawPartialRectContainer sz (upperLeft, 0, countRectContainerChars sz - 1)

{-# INLINABLE drawRectFrameInterpolation #-}
drawRectFrameInterpolation :: (Draw e, MonadReader e m, MonadIO m)
                           => RectContainer
                           -> RectContainer
                           -> Int
                           -> Int
                           -> LayeredColor
                           -> m ()
drawRectFrameInterpolation rf1@(RectContainer sz1 upperLeft1)
                 rf2@(RectContainer sz2 upperLeft2) lastFrame frame color = do
  let (Coords _ (Coord dc)) = diffCoords upperLeft1 upperLeft2
      draw' di1 di2 = do
        let fromRanges = ranges (lastFrame-(frame+di1)) sz1 FromBs
            toRanges   = ranges (lastFrame-(frame+di2)) sz2 FromAs
        mapM_ (drawRectFrameRange rf1 color) fromRanges
        mapM_ (drawRectFrameRange rf2 color) toRanges
  if dc >= 0
    then
      -- expanding animation
      draw' dc 0
    else
      -- shrinking animation
      draw' 0 (negate dc)


{-# INLINABLE drawRectFrameRange #-}
drawRectFrameRange :: (Draw e, MonadReader e m, MonadIO m)
                     => RectContainer
                     -> LayeredColor
                     -> (Int, Int)
                     -> m ()
drawRectFrameRange (RectContainer sz r) color (min_, max_) =
  drawPartialRectContainer sz (r, min_, max_) color

-- | Considering a closed continuous path with an even number of points labeled
--  A and B and alternating along the path : A,B,A,B,A,B
--
-- (Think of a rectangle, the middles of the sides being
-- the A points, the extremities being the B points)
--
--
-- FromBs is the complement, i.e the same as above, but replacing As with Bs and vice-versa.
data BuildFrom = FromAs
               -- ^ First draw A points, then expand the drawn regions
               -- to the right and left of A points, until B points are reached.
               | FromBs
               -- ^ First draw B points, then expand the drawn regions
               -- to the right and left of B points, until A points are reached.

ranges :: Int
       -- ^ Progress of the interpolation
       -> Size
       -- ^ Size of the content, /not/ the container
       -> BuildFrom
       -- ^ The building strategy
       -> [(Int, Int)]
ranges progress sz =
  let h = countRectContainerVerticalChars sz
      w = countRectContainerHorizontalChars sz

      diff = quot (w - h) 2 -- vertical and horizontal animations should start at the same time

      extW = rangeByRemovingFromTotal progress w
      extH = rangeByRemovingFromTotal (max 0 $ progress-diff) h

      exts = [extW, extH, extW, extH]
      lengths = [w,h,w,h]

      (total, starts) = mapAccumL (\acc v -> (acc + v, acc)) 0 lengths
      res = map (\(ext, s) -> ext s) $ zip exts starts
  in \case
        FromAs -> res
        FromBs -> complement 0 (total-1) res

complement :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
complement a max_ []          = [(a, max_)]
complement a max_ ((b,c):rest) = (a, b - 1) : complement (c + 1) max_ rest

rangeByRemovingFromTotal :: Int -> Int -> Int -> (Int, Int)
rangeByRemovingFromTotal remove total start =
  let min_ = remove
      max_ = total - 1 - remove
  in (start + min_, start + max_)

{- | Produces a 'RectContainer' at given horizontal and vertical distances from
another 'RectContainer'.

For example, in this illustration, @cont'@ is at dx = dy = 3 from @cont@:

@
    cont'
    +--------+..-
    |        |  |  dy = 3
    |  cont  |  |
    |  +--+..|..-
    |  |  |  |
    |  |  |  |
    |  +--+  |
    |  .     |
    |  .     |
    +--------+
    .  .
    .  .
   >|--|<
    dx = 3
@
-}
mkRectContainerAtDistance :: RectContainer
                          -- ^ Reference container
                          -> Length Width
                          -- ^ Horizontal distance
                          -> Length Height
                          -- ^ Vertical distance
                          -> RectContainer
mkRectContainerAtDistance (RectContainer (Size y x) upperLeft) dx dy =
  RectContainer (Size (y+2*dy) (x + 2*dx))
  $ sumCoords upperLeft $ toCoords (-dy) (-dx)


{- | Returns points centered on the sides of a container.

[Favored direction for centers of horizontal sides]
When computing the /center/ of an horizontal side, if the side has an /even/ length,
we must favor a 'Direction'.
(Note that if the side has an /odd/ length, there is no ambiguity.)

In 'Text.Alignment.align' implementation, 'Text.Alignment.Centered' alignment
favors the 'RIGHT' 'Direction':

@
   1
   12
  123
  1234
   ^
@


* If we, too, favor the 'RIGHT' 'Direction', when the returned point is used as
reference for a 'Centered' alignment, the text will tend to be too far to the 'RIGHT',
as illustrated here (@^@ indicates the chosen center):

@
   1
 +--+
   12
 +--+
  123
 +--+
  1234
 +--+
   ^
@

* So we will favor the 'LEFT' 'Direction', to counterbalance the choice made in
'Text.Alignment.align' 's implementation:

@
  1
 +--+
  12
 +--+
 123
 +--+
 1234
 +--+
  ^
@
-}
getSideCenters :: RectContainer
               -> (Coords Pos, Coords Pos, Coords Pos, Coords Pos)
               -- ^ (center Up, center Down, center Left, center Right)
getSideCenters (RectContainer (Size rs' cs') upperLeft) =
  (centerUp, centerDown, leftMiddle, rightMiddle)
 where
  -- in both directions, from inner content to outer container:
  rs = rs' + 2
  cs = cs' + 2

  cHalf = quot (cs-1) 2 -- favors 'LEFT' 'Direction', see haddock comments.
  rHalf = quot (rs-1) 2 -- favors 'Up' 'Direction'
  rFull = rs-1

  centerUp    = sumCoords upperLeft $ toCoords 0     cHalf
  centerDown  = sumCoords upperLeft $ toCoords rFull cHalf
  leftMiddle  = sumCoords upperLeft $ toCoords rHalf 0
  rightMiddle = sumCoords upperLeft $ toCoords rHalf (cs-1)


-- | Create a 'RectContainer' whose inner and outter /content/ matches a 'RectArea'.
mkRectContainerWithTotalArea :: RectArea a -> RectContainer
mkRectContainerWithTotalArea rectArea@(RectArea upperLeft _) =
  let (Size h w) = rectAreaSize rectArea
  in RectContainer (Size (h-2) (w-2)) upperLeft

-- | Create a 'RectContainer' whose inner /content/ matches a 'RectArea'.
mkRectContainerWithCenterAndInnerSize :: Coords Pos -> Size -> RectContainer
mkRectContainerWithCenterAndInnerSize center s =
  RectContainer s $ upperLeftFromCenterAndSize center s

upperLeftFromCenterAndSize :: Coords Pos -> Size -> Coords Pos
upperLeftFromCenterAndSize center (Size h w) =
  sumCoords center $ toCoords (-1 - quot h 2) (-1 - quot w 2)
