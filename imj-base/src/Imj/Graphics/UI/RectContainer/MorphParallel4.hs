{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.UI.RectContainer.MorphParallel4
          ( renderPartialRectContainer
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

{-# INLINABLE renderPartialRectContainer #-}
renderPartialRectContainer :: (Draw e, MonadReader e m, MonadIO m)
                           => Size
                           -- ^ Dimensions of the content of the container
                           -> (Coords Pos, Int, Int)
                           -- ^ Coordinates of the upper left corner of the container, from, to.
                           -> LayeredColor
                           -> m ()
renderPartialRectContainer sz r colors =
  renderUpperWall sz colors r
    >>= renderRightWall sz colors
    >>= renderLowerWall sz colors
    >>= renderLeftWall sz colors
    >> return ()

{-# INLINABLE renderLeftWall #-}
renderLeftWall :: (Draw e, MonadReader e m, MonadIO m)
               => Size
               -> LayeredColor
               -> (Coords Pos, Int, Int)
               -> m (Coords Pos, Int, Int)
renderLeftWall = renderSideWall Up

{-# INLINABLE renderRightWall #-}
renderRightWall :: (Draw e, MonadReader e m, MonadIO m)
               => Size
               -> LayeredColor
               -> (Coords Pos, Int, Int)
               -> m (Coords Pos, Int, Int)
renderRightWall = renderSideWall Down

{-# INLINABLE renderSideWall #-}
renderSideWall :: (Draw e, MonadReader e m, MonadIO m)
               => Direction
               -> Size
               -> LayeredColor
               -> (Coords Pos, Int, Int)
               -> m (Coords Pos, Int, Int)
renderSideWall dir sz colors (ref, from, to) = do
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

{-# INLINABLE renderUpperWall #-}
renderUpperWall :: (Draw e, MonadReader e m, MonadIO m)
                => Size
                -> LayeredColor
                -> (Coords Pos, Int, Int)
                -> m (Coords Pos, Int, Int)
renderUpperWall =
  renderHorizontalWall Down RIGHT '_'

{-# INLINABLE renderLowerWall #-}
renderLowerWall :: (Draw e, MonadReader e m, MonadIO m)
                => Size
                -> LayeredColor
                -> (Coords Pos, Int, Int)
                -> m (Coords Pos, Int, Int)
renderLowerWall =
  renderHorizontalWall Up LEFT 'T'

{-# INLINABLE renderHorizontalWall #-}
renderHorizontalWall :: (Draw e, MonadReader e m, MonadIO m)
                     => Direction
                     -> Direction
                     -> Char
                     -> Size
                     -> LayeredColor
                     -> (Coords Pos, Int, Int)
                     -> m (Coords Pos, Int, Int)
renderHorizontalWall dirV dirH char sz colors (upperLeft, from, to) = do
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
