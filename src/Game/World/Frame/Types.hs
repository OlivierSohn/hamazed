{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Game.World.Frame.Types
           ( FrameAnimationParallel4(..)
           , FrameSpec(..)
           ) where


import           Imajuscule.Prelude

import           GHC.Show(showString)

import           Data.List( mapAccumL, zip )

import           Interpolation

import           Game.World.Space.Types

import           Geo.Discrete

import           Render
import           Render.Console

data FrameSpec = FrameSpec {
    _frameSpecSize :: !WorldSize
  , _frameSpecUpperLeft :: !Coords
  , _frameSpecColors :: !Colors
  , _frameSpecBuffers :: !(IORef Buffers)
} deriving(Eq)

instance Show FrameSpec where
  showsPrec _ (FrameSpec a b c _) = showString $ show (a,b,c,"IORef")


newtype FrameAnimationParallel4 = FrameAnimationParallel4 FrameSpec
          deriving(Show)

instance DiscretelyInterpolable FrameAnimationParallel4 where
  distance (FrameAnimationParallel4 (FrameSpec s _ _ _)) (FrameAnimationParallel4 (FrameSpec s' _ _ _)) -- TODO animate colors too
    | s == s'   = 1 -- no animation because sizes are equal
    | otherwise = 1 + quot (1 + max (maxDim s) (maxDim s')) 2

  interpolateIO f@(FrameAnimationParallel4 from)
                t@(FrameAnimationParallel4 to) frame
    | frame <= 0         = renderWhole from
    | frame >= lastFrame = renderWhole to
    | otherwise          = renderTransition from to lastFrame frame
    where
      lastFrame = pred $ distance f t


renderWhole :: FrameSpec -> IO ()
renderWhole (FrameSpec sz upperLeft color b) =
  renderPartialWorldFrame sz color b (upperLeft, 0, countWorldFrameChars sz - 1)

renderTransition :: FrameSpec -> FrameSpec -> Int -> Int -> IO ()
renderTransition from@(FrameSpec _ fromUpperLeft _ _)
                   to@(FrameSpec _ toUpperLeft _ _) n i = do
      let
          (Coords _ (Col dc)) = diffCoords fromUpperLeft toUpperLeft
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
renderFrom rangeType progress (FrameSpec sz r color b) = do
  let rs = ranges progress sz rangeType
  mapM_ (\(min_, max_) -> renderPartialWorldFrame sz color b (r, min_, max_)) rs

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

renderPartialWorldFrame :: WorldSize -> Colors -> IORef Buffers -> (Coords, Int, Int) -> IO ()
renderPartialWorldFrame sz colors b r =
  renderUpperWall sz colors b r
    >>= renderRightWall sz colors b
    >>= renderLowerWall sz colors b
    >>= renderLeftWall sz colors b
    >> return ()

renderRightWall :: WorldSize -> Colors -> IORef Buffers -> (Coords, Int, Int) -> IO (Coords, Int, Int)
renderRightWall sz colors b (upperRight, from, to) = do
  let countMax = countWorldFrameVertical sz
      (actualFrom, actualTo) = actualRange countMax (from, to)
      nChars = 1 + actualTo - actualFrom
      wallCoords = map (\n -> move n Down upperRight) [actualFrom..actualTo]
      nextR = move countMax Down upperRight
  mapM_ (\pos -> drawChar '|' pos colors b) wallCoords
  if nChars <= 0
    then
      return (nextR, from - countMax, to - countMax)
    else
      return (nextR, from + nChars - countMax, to - countMax)

renderLeftWall :: WorldSize -> Colors -> IORef Buffers -> (Coords, Int, Int) -> IO (Coords, Int, Int)
renderLeftWall sz colors b (lowerLeft, from, to) = do
  let countMax = countWorldFrameVertical sz
      (actualFrom, actualTo) = actualRange countMax (from, to)
      nChars = 1 + actualTo - actualFrom
      wallCoords = map (\n -> move n Up lowerLeft) [actualFrom..actualTo]
      nextR = move countMax Up lowerLeft
  mapM_ (\pos -> drawChar '|' pos colors b) wallCoords
  if nChars <= 0
    then
      return (nextR, from - countMax, to - countMax)
    else
      return (nextR, from + nChars - countMax, to - countMax)

-- 0 is upper left
renderUpperWall :: WorldSize -> Colors -> IORef Buffers -> (Coords, Int, Int) -> IO (Coords, Int, Int)
renderUpperWall sz colors b (upperLeft, from, to) = do
  let countMax = countWorldFrameHorizontal sz
      (actualFrom, actualTo) = actualRange countMax (from, to)
      nChars = 1 + actualTo - actualFrom
      nextR = translateInDir Down $ move (countMax - 1) RIGHT upperLeft
  if nChars <= 0
    then
      return (nextR, from - countMax, to - countMax)
    else
      drawChars nChars '_' (move actualFrom RIGHT upperLeft) colors b
       >> return (nextR, from + nChars - countMax, to - countMax)

renderLowerWall :: WorldSize -> Colors -> IORef Buffers -> (Coords, Int, Int) -> IO (Coords, Int, Int)
renderLowerWall sz colors b (lowerRight, from, to) = do
  let countMax = countWorldFrameHorizontal sz
      (actualFrom, actualTo) = actualRange countMax (from, to)
      nChars = 1 + actualTo - actualFrom
      nextR = translateInDir Up $ move (countMax - 1) LEFT lowerRight
  if nChars <= 0
    then
      return (nextR, from - countMax, to - countMax)
    else
      drawChars nChars 'T' (move actualTo LEFT lowerRight) colors b
       >> return (nextR, from + nChars - countMax, to - countMax)


actualRange :: Int -> (Int, Int) -> (Int, Int)
actualRange countMax (from, to) =
  (max 0 from, min to $ pred countMax)
