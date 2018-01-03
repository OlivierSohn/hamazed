{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.UI.RectContainer.MorphParallel4
          ( drawPartialRectContainer
          , countRectContainerHorizontalChars
          , countRectContainerVerticalChars
          , countRectContainerChars
          ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import           Imj.Graphics.Render
import           Imj.Geo.Discrete


countRectContainerChars :: Size -> Int
countRectContainerChars s =
  2 * countRectContainerHorizontalChars s + 2 * countRectContainerVerticalChars s

countRectContainerHorizontalChars :: Size -> Int
countRectContainerHorizontalChars (Size _ cs) =
  fromIntegral cs + 2

countRectContainerVerticalChars :: Size -> Int
countRectContainerVerticalChars (Size rs _) =
  fromIntegral rs

{-# INLINABLE drawPartialRectContainer #-}
drawPartialRectContainer :: (Draw e, MonadReader e m, MonadIO m)
                           => Size
                           -- ^ Dimensions of the content of the container
                           -> (Coords Pos, Int, Int)
                           -- ^ Coordinates of the upper left corner of the container, from, to.
                           -> LayeredColor
                           -> m ()
drawPartialRectContainer sz r colors =
  drawUpperWall sz colors r
    >>= drawRightWall sz colors
    >>= drawLowerWall sz colors
    >>= drawLeftWall sz colors
    >> return ()

{-# INLINABLE drawLeftWall #-}
drawLeftWall :: (Draw e, MonadReader e m, MonadIO m)
               => Size
               -> LayeredColor
               -> (Coords Pos, Int, Int)
               -> m (Coords Pos, Int, Int)
drawLeftWall = drawSideWall Up

{-# INLINABLE drawRightWall #-}
drawRightWall :: (Draw e, MonadReader e m, MonadIO m)
               => Size
               -> LayeredColor
               -> (Coords Pos, Int, Int)
               -> m (Coords Pos, Int, Int)
drawRightWall = drawSideWall Down

{-# INLINABLE drawSideWall #-}
drawSideWall :: (Draw e, MonadReader e m, MonadIO m)
               => Direction
               -> Size
               -> LayeredColor
               -> (Coords Pos, Int, Int)
               -> m (Coords Pos, Int, Int)
drawSideWall dir sz colors (ref, from, to) = do
  let countMax = countRectContainerVerticalChars sz
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

{-# INLINABLE drawUpperWall #-}
drawUpperWall :: (Draw e, MonadReader e m, MonadIO m)
                => Size
                -> LayeredColor
                -> (Coords Pos, Int, Int)
                -> m (Coords Pos, Int, Int)
drawUpperWall =
  drawHorizontalWall Down RIGHT '_'

{-# INLINABLE drawLowerWall #-}
drawLowerWall :: (Draw e, MonadReader e m, MonadIO m)
                => Size
                -> LayeredColor
                -> (Coords Pos, Int, Int)
                -> m (Coords Pos, Int, Int)
drawLowerWall =
  drawHorizontalWall Up LEFT 'T'

{-# INLINABLE drawHorizontalWall #-}
drawHorizontalWall :: (Draw e, MonadReader e m, MonadIO m)
                     => Direction
                     -> Direction
                     -> Char
                     -> Size
                     -> LayeredColor
                     -> (Coords Pos, Int, Int)
                     -> m (Coords Pos, Int, Int)
drawHorizontalWall dirV dirH char sz colors (upperLeft, from, to) = do
  let countMax = countRectContainerHorizontalChars sz
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
