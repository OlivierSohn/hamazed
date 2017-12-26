{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.UI.RectFrame
        (
          -- * RectFrame
          -- | 'RectFrame' Represents a UI rectangular frame of discrete dimensions.
          RectFrame(..)
        ) where

import           Imj.Prelude

import           Data.List( mapAccumL, zip )
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import           Imj.Draw
import           Imj.Geo.Discrete
import           Imj.Interpolation
import           Imj.UI.RectFrame.InterpolateParallel4


data RectFrame = RectFrame {
    _rectFrameSize :: !Size
    -- ^ Width & Height
  , _rectFrameUpperLeft :: !Coords
    -- ^ Upper left corner
  , _rectFrameColors :: !LayeredColor
    -- ^ Foreground and background colors
} deriving(Eq, Show)


-- | Smoothly transforms the 4 sides of the rectangle simultaneously, from their middle
-- to their extremities.
instance DiscretelyInterpolable RectFrame where
  distance (RectFrame s  _ _ )
           (RectFrame s' _ _ ) -- TODO animate colors too
    | s == s'   = 1 -- no animation because sizes are equal
    | otherwise = 1 + quot (1 + max (maxLength s) (maxLength s')) 2

  {-# INLINABLE interpolateIO #-}
  interpolateIO from to frame
    | frame <= 0         = renderWhole from
    | frame >= lastFrame = renderWhole to
    | otherwise          = renderRectFrameInterpolation from to lastFrame frame
    where
      lastFrame = pred $ distance from to

{-# INLINABLE renderWhole #-}
renderWhole :: (Draw e, MonadReader e m, MonadIO m)
            => RectFrame
            -> m ()
renderWhole (RectFrame sz upperLeft color) =
  renderPartialRectFrame sz color (upperLeft, 0, countWorldFrameChars sz - 1)

{-# INLINABLE renderRectFrameInterpolation #-}
renderRectFrameInterpolation :: (Draw e, MonadReader e m, MonadIO m)
                             => RectFrame
                             -> RectFrame
                             -> Int
                             -> Int
                             -> m ()
renderRectFrameInterpolation rf1@(RectFrame sz1 upperLeft1 _)
                 rf2@(RectFrame sz2 upperLeft2 _) lastFrame frame = do
  let (Coords _ (Coord dc)) = diffCoords upperLeft1 upperLeft2
      render di1 di2 = do
        let fromRanges = ranges (lastFrame-(frame+di1)) sz1 Extremities
            toRanges   = ranges (lastFrame-(frame+di2)) sz2 Middle
        mapM_ (renderRectFrameRange rf1) fromRanges
        mapM_ (renderRectFrameRange rf2) toRanges
  if dc >= 0
    then
      -- expanding animation
      render dc 0
    else
      -- shrinking animation
      render 0 (negate dc)


{-# INLINABLE renderRectFrameRange #-}
renderRectFrameRange :: (Draw e, MonadReader e m, MonadIO m)
                     => RectFrame
                     -> (Int, Int)
                     -> m ()
renderRectFrameRange (RectFrame sz r color) (min_, max_) =
  renderPartialRectFrame sz color (r, min_, max_)




data BuildFrom = Middle
               | Extremities -- generates the complement

ranges :: Int -> Size -> BuildFrom -> [(Int, Int)]
ranges progress sz =
  let h = countWorldFrameVertical sz
      w = countWorldFrameHorizontal sz

      diff = quot (w - h) 2 -- vertical and horizontal animations should start at the same time

      extW = rangeByRemovingFromTotal progress w
      extH = rangeByRemovingFromTotal (max 0 $ progress-diff) h

      exts = [extW, extH, extW, extH]
      lengths = [w,h,w,h]

      (total, starts) = mapAccumL (\acc v -> (acc + v, acc)) 0 lengths
      res = map (\(ext, s) -> ext s) $ zip exts starts
  in \case
        Middle      -> res
        Extremities -> complement 0 (total-1) res

complement :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
complement a max_ []          = [(a, max_)]
complement a max_ l@((b,c):_) = (a, pred b) : complement (succ c) max_ (tail l)

rangeByRemovingFromTotal :: Int -> Int -> Int -> (Int, Int)
rangeByRemovingFromTotal remove total start =
  let min_ = remove
      max_ = total - 1 - remove
  in (start + min_, start + max_)
