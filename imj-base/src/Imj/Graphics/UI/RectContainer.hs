{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Graphics.UI.RectContainer
        (
          -- * RectContainer
            {- | 'RectContainer' represents a rectangular UI container. It
            contains the 'Size' of its /content/, and an upper left coordinate.

            Being 'Colorable', it can be wrapped in a 'Colored' to gain the notion of color. -}
          RectContainer(..)
        , getSideCentersAtDistance
          -- * Reexports
        , Colorable(..)
        ) where

import           Imj.Prelude

import           Data.List( mapAccumL, zip )
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import           Imj.Graphics.Render
import           Imj.Geo.Discrete
import           Imj.Graphics.UI.RectContainer.MorphParallel4
import           Imj.Graphics.Class.DiscreteColorableMorphing

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
    _rectFrameContentSize :: !Size
    -- ^ /Content/ size.
  , _rectFrameUpperLeft :: !(Coords Pos)
    -- ^ Upper left corner.
} deriving(Eq, Show)

-- TODO notion "continuous closed path" to factor 'ranges' and 'renderRectFrameInterpolation' logics.

instance Colorable RectContainer where
  drawUsingColor = renderWhole
  {-# INLINABLE drawUsingColor #-}

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
    | otherwise          = renderRectFrameInterpolation from to lastFrame frame color
    where
      lastFrame = pred $ distance from to

{-# INLINABLE renderWhole #-}
renderWhole :: (Draw e, MonadReader e m, MonadIO m)
            => RectContainer
            -> LayeredColor
            -> m ()
renderWhole (RectContainer sz upperLeft) =
  renderPartialRectContainer sz (upperLeft, 0, countRectContainerChars sz - 1)

{-# INLINABLE renderRectFrameInterpolation #-}
renderRectFrameInterpolation :: (Draw e, MonadReader e m, MonadIO m)
                             => RectContainer
                             -> RectContainer
                             -> Int
                             -> Int
                             -> LayeredColor
                             -> m ()
renderRectFrameInterpolation rf1@(RectContainer sz1 upperLeft1)
                 rf2@(RectContainer sz2 upperLeft2) lastFrame frame color = do
  let (Coords _ (Coord dc)) = diffCoords upperLeft1 upperLeft2
      render di1 di2 = do
        let fromRanges = ranges (lastFrame-(frame+di1)) sz1 FromBs
            toRanges   = ranges (lastFrame-(frame+di2)) sz2 FromAs
        mapM_ (renderRectFrameRange rf1 color) fromRanges
        mapM_ (renderRectFrameRange rf2 color) toRanges
  if dc >= 0
    then
      -- expanding animation
      render dc 0
    else
      -- shrinking animation
      render 0 (negate dc)


{-# INLINABLE renderRectFrameRange #-}
renderRectFrameRange :: (Draw e, MonadReader e m, MonadIO m)
                     => RectContainer
                     -> LayeredColor
                     -> (Int, Int)
                     -> m ()
renderRectFrameRange (RectContainer sz r) color (min_, max_) =
  renderPartialRectContainer sz (r, min_, max_) color

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
complement a max_ l@((b,c):_) = (a, pred b) : complement (succ c) max_ (tail l)

rangeByRemovingFromTotal :: Int -> Int -> Int -> (Int, Int)
rangeByRemovingFromTotal remove total start =
  let min_ = remove
      max_ = total - 1 - remove
  in (start + min_, start + max_)


-- TODO split : function to make the container at a distance, and function to take the centers.
{- | Returns points centered on the sides of a container which is at a given distances
(dx and dy) from the reference container.

[container at a distance from another container]
In this illustration, @cont'@ is at dx = dy = 3 from @cont@:

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
getSideCentersAtDistance :: RectContainer
                         -- ^ Reference container
                         -> Length Width
                         -- ^ Horizontal distance
                         -> Length Height
                         -- ^ Horizontal distance
                         -> (Coords Pos, Coords Pos, Coords Pos, Coords Pos)
                         -- ^ (center Up, center Down, center Left, center Right)
getSideCentersAtDistance (RectContainer (Size rs' cs') upperLeft') dx dy =
  (centerUp, centerDown, leftMiddle, rightMiddle)
 where
  deltaLength dist =
    2 *    -- in both directions
      (1 +   -- from inner content to outer container
       dist) -- from container to container'
  rs = rs' + fromIntegral (deltaLength dy)
  cs = cs' + fromIntegral (deltaLength dx)
  upperLeft = translate' (fromIntegral $ -dy) (fromIntegral $ -dx) upperLeft'

  cHalf = quot (cs-1) 2 -- favors 'LEFT' 'Direction', see haddock comments.
  rHalf = quot (rs-1) 2 -- favors 'Up' 'Direction'
  rFull = rs-1

  centerUp    = translate' 0     cHalf upperLeft
  centerDown  = translate' rFull cHalf upperLeft
  leftMiddle  = translate' rHalf 0     upperLeft
  rightMiddle = translate' rHalf (cs-1) upperLeft
