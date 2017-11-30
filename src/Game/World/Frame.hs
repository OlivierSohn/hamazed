{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Game.World.Frame
    ( renderWorldFrame
    , FrameAnimation(..)
    , maxNumberOfSteps
    ) where

import           Imajuscule.Prelude

import           Data.List( mapAccumL )

import           Color

import           Game.World.Size

import           Geo.Discrete hiding( move )

import           Render
import           Render.Console

import           Timing

data FrameAnimation = FrameAnimation {
    _frameAnimationPrevSize :: !WorldSize
  , _frameAnimationProgress :: !Int
  , _frameAnimationDeadline :: !KeyTime
}

countWorldFrameChars :: WorldSize -> Int
countWorldFrameChars s =
  2 * countWorlFrameHorizontal s + 2 * countWorlFrameVertical s

countWorlFrameHorizontal :: WorldSize -> Int
countWorlFrameHorizontal (WorldSize (Coords _ (Col cs))) =
  cs + 2

countWorlFrameVertical :: WorldSize -> Int
countWorlFrameVertical (WorldSize (Coords (Row rs) _)) =
  rs

renderPartialWorldFrame :: WorldSize -> (RenderState, Int, Int) -> IO ()
renderPartialWorldFrame sz r =
  renderUpperWall sz r
    >>= renderRightWall sz
    >>= renderLowerWall sz
    >>= renderLeftWall sz
    >> return ()

renderRightWall :: WorldSize -> (RenderState, Int, Int) -> IO (RenderState, Int, Int)
renderRightWall sz (upperRight, from, to) = do
  let countMax = countWorlFrameVertical sz
      (actualFrom, actualTo) = actualRange countMax (from, to)
      countChars = 1 + actualTo - actualFrom
      rightWallCoords = map (\n -> move n Down upperRight) [actualFrom..actualTo]
      nextR = move countMax Down upperRight
  mapM_ (renderChar_ '|') rightWallCoords
  if countChars <= 0
    then
      return (nextR, from - countMax, to - countMax)
    else
      return (nextR, from + countChars - countMax, to - countMax)

renderLeftWall :: WorldSize -> (RenderState, Int, Int) -> IO (RenderState, Int, Int)
renderLeftWall sz (lowerLeft, from, to) = do
  let countMax = countWorlFrameVertical sz
      (actualFrom, actualTo) = actualRange countMax (from, to)
      countChars = 1 + actualTo - actualFrom
      leftWallCoords = map (\n -> move n Up lowerLeft) [actualFrom..actualTo]
      nextR = move countMax Up lowerLeft
  mapM_ (renderChar_ '|') leftWallCoords
  if countChars <= 0
    then
      return (nextR, from - countMax, to - countMax)
    else
      return (nextR, from + countChars - countMax, to - countMax)

-- 0 is upper left
renderUpperWall :: WorldSize -> (RenderState, Int, Int) -> IO (RenderState, Int, Int)
renderUpperWall sz (upperLeft, from, to) = do
  let countMax = countWorlFrameHorizontal sz
      (actualFrom, actualTo) = actualRange countMax (from, to)
      countChars = 1 + actualTo - actualFrom
      nextR = go Down $ move (countMax - 1) RIGHT upperLeft
  if countChars <= 0
    then
      return (nextR, from - countMax, to - countMax)
    else
      renderChars countChars '_' (move actualFrom RIGHT upperLeft)
       >> return (nextR, from + countChars - countMax, to - countMax)

renderLowerWall :: WorldSize -> (RenderState, Int, Int) -> IO (RenderState, Int, Int)
renderLowerWall sz (lowerRight, from, to) = do
  let countMax = countWorlFrameHorizontal sz
      (actualFrom, actualTo) = actualRange countMax (from, to)
      countChars = 1 + actualTo - actualFrom
      nextR = go Up $ move (countMax - 1) LEFT lowerRight
  if countChars <= 0
    then
      return (nextR, from - countMax, to - countMax)
    else
      renderChars countChars 'T' (move actualTo LEFT lowerRight)
       >> return (nextR, from + countChars - countMax, to - countMax)

actualRange :: Int -> (Int, Int) -> (Int, Int)
actualRange countMax (from, to) =
  (max 0 from, min to $ pred countMax)

renderWorldFrame :: Maybe FrameAnimation -- ^ contains previous size
                 -> WorldSize -- ^ new size
                 -> RenderState -- ^ wrt. new size
                 -> IO RenderState
renderWorldFrame mayAnim sz upperLeft = do
  fg <- setRawForeground worldFrameColor
  maybe
    (renderPartialWorldFrame sz (upperLeft, 0, countWorldFrameChars sz - 1))
    (\(FrameAnimation szBefore i _) -> do
      let diff@(RenderState (Coords (Row dr) (Col dc))) = diffUpperLeft sz szBefore
      if dc >= 0
        then do
          -- expanding animation
          let n = maxNumberOfSteps sz szBefore
          renderFromMiddle FromExtremities (n-(i+dc)) szBefore $ sumRS diff upperLeft
          renderFromMiddle FromMiddle (n-i) sz upperLeft
        else do
          -- shrinking animation
          let n = maxNumberOfSteps sz szBefore
          renderFromMiddle FromMiddle (n-(i-dc)) sz upperLeft
          renderFromMiddle FromExtremities (n-i) szBefore $ sumRS diff upperLeft
    ) mayAnim
  restoreForeground fg
  return $ go Down $ go RIGHT upperLeft

maxNumberOfSteps :: WorldSize -> WorldSize -> Int
maxNumberOfSteps s s' = 1 + quot (max (maxDim s) (maxDim s')) 2

augment :: WorldSize -> WorldSize
augment (WorldSize (Coords r c)) =
  WorldSize (Coords (r+2) (c+2))

reduce :: WorldSize -> WorldSize
reduce (WorldSize (Coords r c)) =
  WorldSize (Coords (r-2) (c-2))

data RangeType = FromMiddle
               | FromExtremities -- generates the complement

ranges :: Int -> WorldSize -> RangeType -> [(Int, Int)]
ranges progress sz =
  let h = countWorlFrameVertical sz
      w = countWorlFrameHorizontal sz
      ext = rangeByRemovingFromTotal progress

      lengths = [w,h,w,h]
      (total, starts) = mapAccumL (\acc v -> (acc + v, acc)) 0 lengths
      res = zipWith ext lengths starts
  in  \case
        FromMiddle -> res
        FromExtremities -> complement 0 (total-1) res

renderFromMiddle :: RangeType -> Int -> WorldSize -> RenderState -> IO ()
renderFromMiddle rangeType progress sz r = do
  let rs = ranges progress sz rangeType
  mapM_ (\(min_, max_) -> renderPartialWorldFrame sz (r, min_, max_)) rs

complement :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
complement a max_ []          = [(a, max_)]
complement a max_ l@((b,c):_) = (a, pred b) : complement (succ c) max_ (tail l)

rangeByRemovingFromTotal :: Int -> Int -> Int -> (Int, Int)
rangeByRemovingFromTotal remove total start =
  let min_ = remove
      max_ = total - 1 - remove
  in (start + min_, start + max_)

rangeWithRadiusCenter :: Int -> Int -> (Int, Int)
rangeWithRadiusCenter radius center = (center-radius, center+radius)
