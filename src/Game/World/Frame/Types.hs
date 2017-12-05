{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Game.World.Frame.Types
           ( FrameAnimationParallel4(..)
           , FrameSpec(..)
           ) where


import           Imajuscule.Prelude

import           Data.List( mapAccumL, zip )

import           Interpolation

import           Game.World.Space.Types

import           Render
import           Render.Console

data FrameSpec = FrameSpec {
    _frameSpecSize :: !WorldSize
  , _frameSpecUpperLeft :: !RenderState
} deriving(Eq, Show)


newtype FrameAnimationParallel4 = FrameAnimationParallel4 FrameSpec

instance DiscretelyInterpolable FrameAnimationParallel4 where
  distance (FrameAnimationParallel4 (FrameSpec s _)) (FrameAnimationParallel4 (FrameSpec s' _ ))
    | s == s'   = 1 -- no animation because sizes are equal
    | otherwise = 1 + quot (1 + max (maxDim s) (maxDim s')) 2

  interpolateIO f@(FrameAnimationParallel4 from)
                t@(FrameAnimationParallel4 to) frame
    | frame <= 0 = renderWhole from
    | frame >= lastFrame = renderWhole to
    | otherwise  = renderTransition from to lastFrame frame
      where lastFrame = pred $ distance f t


renderWhole :: FrameSpec -> IO ()
renderWhole (FrameSpec sz upperLeft) =
  renderPartialWorldFrame sz (upperLeft, 0, countWorldFrameChars sz - 1)

renderTransition :: FrameSpec -> FrameSpec -> Int -> Int -> IO ()
renderTransition from@(FrameSpec _ fromUpperLeft) to@(FrameSpec _ toUpperLeft) n i = do
      let
          (RenderState (Coords _ (Col dc))) = diffRS fromUpperLeft toUpperLeft
          render diBefore di = do
            renderFrom Extremities (n-(i+diBefore)) from
            renderFrom Middle      (n-(i+di))       to
      if dc >= 0
        then
          -- expanding animation
          render dc 0
        else
          -- shrinking animation
          render 0 (negate dc)

data BuildFrom = Middle
               | Extremities -- generates the complement

ranges :: Int -> WorldSize -> BuildFrom -> [(Int, Int)]
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

renderFrom :: BuildFrom -> Int -> FrameSpec -> IO ()
renderFrom rangeType progress (FrameSpec sz r) = do
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


countWorldFrameChars :: WorldSize -> Int
countWorldFrameChars s =
  2 * countWorldFrameHorizontal s + 2 * countWorldFrameVertical s

countWorldFrameHorizontal :: WorldSize -> Int
countWorldFrameHorizontal (WorldSize (Coords _ (Col cs))) =
  cs + 2

countWorldFrameVertical :: WorldSize -> Int
countWorldFrameVertical (WorldSize (Coords (Row rs) _)) =
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
  let countMax = countWorldFrameVertical sz
      (actualFrom, actualTo) = actualRange countMax (from, to)
      nChars = 1 + actualTo - actualFrom
      rightWallCoords = map (\n -> move n Down upperRight) [actualFrom..actualTo]
      nextR = move countMax Down upperRight
  mapM_ (renderChar_ '|') rightWallCoords
  if nChars <= 0
    then
      return (nextR, from - countMax, to - countMax)
    else
      return (nextR, from + nChars - countMax, to - countMax)

renderLeftWall :: WorldSize -> (RenderState, Int, Int) -> IO (RenderState, Int, Int)
renderLeftWall sz (lowerLeft, from, to) = do
  let countMax = countWorldFrameVertical sz
      (actualFrom, actualTo) = actualRange countMax (from, to)
      nChars = 1 + actualTo - actualFrom
      leftWallCoords = map (\n -> move n Up lowerLeft) [actualFrom..actualTo]
      nextR = move countMax Up lowerLeft
  mapM_ (renderChar_ '|') leftWallCoords
  if nChars <= 0
    then
      return (nextR, from - countMax, to - countMax)
    else
      return (nextR, from + nChars - countMax, to - countMax)

-- 0 is upper left
renderUpperWall :: WorldSize -> (RenderState, Int, Int) -> IO (RenderState, Int, Int)
renderUpperWall sz (upperLeft, from, to) = do
  let countMax = countWorldFrameHorizontal sz
      (actualFrom, actualTo) = actualRange countMax (from, to)
      nChars = 1 + actualTo - actualFrom
      nextR = go Down $ move (countMax - 1) RIGHT upperLeft
  if nChars <= 0
    then
      return (nextR, from - countMax, to - countMax)
    else
      renderChars nChars '_' (move actualFrom RIGHT upperLeft)
       >> return (nextR, from + nChars - countMax, to - countMax)

renderLowerWall :: WorldSize -> (RenderState, Int, Int) -> IO (RenderState, Int, Int)
renderLowerWall sz (lowerRight, from, to) = do
  let countMax = countWorldFrameHorizontal sz
      (actualFrom, actualTo) = actualRange countMax (from, to)
      nChars = 1 + actualTo - actualFrom
      nextR = go Up $ move (countMax - 1) LEFT lowerRight
  if nChars <= 0
    then
      return (nextR, from - countMax, to - countMax)
    else
      renderChars nChars 'T' (move actualTo LEFT lowerRight)
       >> return (nextR, from + nChars - countMax, to - countMax)


actualRange :: Int -> (Int, Int) -> (Int, Int)
actualRange countMax (from, to) =
  (max 0 from, min to $ pred countMax)
