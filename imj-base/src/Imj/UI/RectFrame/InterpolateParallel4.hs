{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.UI.RectFrame.InterpolateParallel4
          ( renderPartialRectFrame
          , countWorldFrameHorizontal
          , countWorldFrameVertical
          , countWorldFrameChars
          ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import           Imj.Draw
import           Imj.Geo.Discrete



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
