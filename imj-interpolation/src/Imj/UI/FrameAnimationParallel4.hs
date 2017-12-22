{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.UI.FrameAnimationParallel4
           (
           -- * FrameAnimationParallel4
           {- | Wraps a 'RectFrame' to make it interpolable through the 'DiscretelyInterpolable'
            instance.

            The transformation happens on all sides simultaneously, from the
            middle to extremities.
           -}
             FrameAnimationParallel4(..)
           -- * Reexports
           , RectFrame(..)
           ) where


import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import           Data.List( mapAccumL, zip )

import           Imj.Draw
import           Imj.Geo.Discrete
import           Imj.Interpolation
import           Imj.UI.RectFrame


-- | Wrapper type on 'RectFrame' to make it interpolable.
newtype FrameAnimationParallel4 = FrameAnimationParallel4 RectFrame
  deriving(Show)

instance DiscretelyInterpolable FrameAnimationParallel4 where
  distance (FrameAnimationParallel4 (RectFrame s  _ _ ))
           (FrameAnimationParallel4 (RectFrame s' _ _ )) -- TODO animate colors too
    | s == s'   = 1 -- no animation because sizes are equal
    | otherwise = 1 + quot (1 + max (maxDim s) (maxDim s')) 2

  {-# INLINABLE interpolateIO #-}
  interpolateIO f@(FrameAnimationParallel4 from) t@(FrameAnimationParallel4 to) frame
    | frame <= 0         = renderWhole from
    | frame >= lastFrame = renderWhole to
    | otherwise          = renderRectFrameInterpolation from to lastFrame frame
    where
      lastFrame = pred $ distance f t

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

{-# INLINABLE renderRectFrameRange #-}
renderRectFrameRange :: (Draw e, MonadReader e m, MonadIO m)
                     => RectFrame
                     -> (Int, Int)
                     -> m ()
renderRectFrameRange (RectFrame sz r color) (min_, max_) =
  renderPartialRectFrame sz color (r, min_, max_)

complement :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
complement a max_ []          = [(a, max_)]
complement a max_ l@((b,c):_) = (a, pred b) : complement (succ c) max_ (tail l)

rangeByRemovingFromTotal :: Int -> Int -> Int -> (Int, Int)
rangeByRemovingFromTotal remove total start =
  let min_ = remove
      max_ = total - 1 - remove
  in (start + min_, start + max_)


countWorldFrameChars :: Size -> Int
countWorldFrameChars s =
  2 * countWorldFrameHorizontal s + 2 * countWorldFrameVertical s

countWorldFrameHorizontal :: Size -> Int
countWorldFrameHorizontal (Size _ cs) =
  fromIntegral cs + 2

countWorldFrameVertical :: Size -> Int
countWorldFrameVertical (Size rs _) =
  fromIntegral rs

{-# INLINABLE renderPartialRectFrame #-}
renderPartialRectFrame :: (Draw e, MonadReader e m, MonadIO m)
                       => Size
                       -> LayeredColor
                       -> (Coords, Int, Int)
                       -> m ()
renderPartialRectFrame sz colors r =
  renderUpperWall sz colors r
    >>= renderRightWall sz colors
    >>= renderLowerWall sz colors
    >>= renderLeftWall sz colors
    >> return ()

{-# INLINABLE renderLeftWall #-}
renderLeftWall :: (Draw e, MonadReader e m, MonadIO m)
               => Size
               -> LayeredColor
               -> (Coords, Int, Int)
               -> m (Coords, Int, Int)
renderLeftWall = renderSideWall Up

{-# INLINABLE renderRightWall #-}
renderRightWall :: (Draw e, MonadReader e m, MonadIO m)
               => Size
               -> LayeredColor
               -> (Coords, Int, Int)
               -> m (Coords, Int, Int)
renderRightWall = renderSideWall Down

{-# INLINABLE renderSideWall #-}
renderSideWall :: (Draw e, MonadReader e m, MonadIO m)
               => Direction
               -> Size
               -> LayeredColor
               -> (Coords, Int, Int)
               -> m (Coords, Int, Int)
renderSideWall dir sz colors (ref, from, to) = do
  let countMax = countWorldFrameVertical sz
      (actualFrom, actualTo) = actualRange countMax (from, to)
      nChars = 1 + actualTo - actualFrom
      wallCoords = map (\n -> move n dir ref) [actualFrom..actualTo]
      nextRef = move countMax dir ref
  mapM_ (\pos -> drawChar '|' pos colors) wallCoords
  if nChars <= 0
    then
      return (nextRef, from - countMax, to - countMax)
    else
      return (nextRef, from + nChars - countMax, to - countMax)

{-# INLINABLE renderUpperWall #-}
renderUpperWall :: (Draw e, MonadReader e m, MonadIO m)
                => Size
                -> LayeredColor
                -> (Coords, Int, Int)
                -> m (Coords, Int, Int)
renderUpperWall =
  renderHorizontalWall Down RIGHT '_'

{-# INLINABLE renderLowerWall #-}
renderLowerWall :: (Draw e, MonadReader e m, MonadIO m)
                => Size
                -> LayeredColor
                -> (Coords, Int, Int)
                -> m (Coords, Int, Int)
renderLowerWall =
  renderHorizontalWall Up LEFT 'T'

{-# INLINABLE renderHorizontalWall #-}
renderHorizontalWall :: (Draw e, MonadReader e m, MonadIO m)
                     => Direction
                     -> Direction
                     -> Char
                     -> Size
                     -> LayeredColor
                     -> (Coords, Int, Int)
                     -> m (Coords, Int, Int)
renderHorizontalWall dirV dirH char sz colors (upperLeft, from, to) = do
  let countMax = countWorldFrameHorizontal sz
      (actualFrom, actualTo) = actualRange countMax (from, to)
      nChars = 1 + actualTo - actualFrom
      nextR = translateInDir dirV $ move (countMax - 1) dirH upperLeft
      startDraw = case dirH of
            RIGHT -> move actualFrom RIGHT upperLeft
            LEFT  -> move actualTo LEFT upperLeft
            _ -> error "not allowed"
  if nChars <= 0
    then
      return (nextR, from - countMax, to - countMax)
    else
      drawChars nChars char startDraw colors
       >> return (nextR, from + nChars - countMax, to - countMax)


actualRange :: Int -> (Int, Int) -> (Int, Int)
actualRange countMax (from, to) =
  (max 0 from, min to $ pred countMax)
