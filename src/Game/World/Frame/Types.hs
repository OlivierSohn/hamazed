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

import           Geo.Discrete

import           Draw


data FrameSpec = FrameSpec {
    _frameSpecSize :: !WorldSize
  , _frameSpecUpperLeft :: !Coords
  , _frameSpecColors :: !LayeredColor
} deriving(Eq, Show)


newtype FrameAnimationParallel4 = FrameAnimationParallel4 FrameSpec
          deriving(Show)

instance DiscretelyInterpolable FrameAnimationParallel4 where
  distance (FrameAnimationParallel4 (FrameSpec s _ _ )) (FrameAnimationParallel4 (FrameSpec s' _ _ )) -- TODO animate colors too
    | s == s'   = 1 -- no animation because sizes are equal
    | otherwise = 1 + quot (1 + max (maxDim s) (maxDim s')) 2

  {-# INLINABLE interpolateIO #-}
  interpolateIO f@(FrameAnimationParallel4 from) t@(FrameAnimationParallel4 to) frame
    | frame <= 0         = renderWhole from
    | frame >= lastFrame = renderWhole to
    | otherwise          = renderTransition from to lastFrame frame
    where
      lastFrame = pred $ distance f t

{-# INLINABLE renderWhole #-}
renderWhole :: (Draw e) => FrameSpec -> ReaderT e IO ()
renderWhole (FrameSpec sz upperLeft color) =
  renderPartialWorldFrame sz color (upperLeft, 0, countWorldFrameChars sz - 1)

{-# INLINABLE renderTransition #-}
renderTransition :: (Draw e) => FrameSpec -> FrameSpec -> Int -> Int -> ReaderT e IO ()
renderTransition from@(FrameSpec _ fromUpperLeft _)
                 to@(FrameSpec _ toUpperLeft _) n i = do
      let (Coords _ dc') = diffCoords fromUpperLeft toUpperLeft
          dc = fromIntegral dc'
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

{-# INLINABLE renderFrom #-}
renderFrom :: (Draw e) => BuildFrom -> Int -> FrameSpec -> ReaderT e IO ()
renderFrom rangeType progress (FrameSpec sz r color) = do
  let rs = ranges progress sz rangeType
  mapM_ (\(min_, max_) -> renderPartialWorldFrame sz color (r, min_, max_)) rs

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
countWorldFrameHorizontal (WorldSize (Coords _ cs)) =
  fromIntegral cs + 2

countWorldFrameVertical :: WorldSize -> Int
countWorldFrameVertical (WorldSize (Coords rs _)) =
  fromIntegral rs

{-# INLINABLE renderPartialWorldFrame #-}
renderPartialWorldFrame :: (Draw e) => WorldSize -> LayeredColor -> (Coords, Int, Int) -> ReaderT e IO ()
renderPartialWorldFrame sz colors r =
  renderUpperWall sz colors r
    >>= renderRightWall sz colors
    >>= renderLowerWall sz colors
    >>= renderLeftWall sz colors
    >> return ()

-- TODO factorize Right with LEft, Upper with Lower

{-# INLINABLE renderRightWall #-}
renderRightWall :: (Draw e) => WorldSize -> LayeredColor -> (Coords, Int, Int) -> ReaderT e IO (Coords, Int, Int)
renderRightWall sz colors (upperRight, from, to) = do
  let countMax = countWorldFrameVertical sz
      (actualFrom, actualTo) = actualRange countMax (from, to)
      nChars = 1 + actualTo - actualFrom
      wallCoords = map (\n -> move n Down upperRight) [actualFrom..actualTo]
      nextR = move countMax Down upperRight
  mapM_ (\pos -> drawChar '|' pos colors) wallCoords
  if nChars <= 0
    then
      return (nextR, from - countMax, to - countMax)
    else
      return (nextR, from + nChars - countMax, to - countMax)

{-# INLINABLE renderLeftWall #-}
renderLeftWall :: (Draw e) => WorldSize -> LayeredColor -> (Coords, Int, Int) -> ReaderT e IO (Coords, Int, Int)
renderLeftWall sz colors (lowerLeft, from, to) = do
  let countMax = countWorldFrameVertical sz
      (actualFrom, actualTo) = actualRange countMax (from, to)
      nChars = 1 + actualTo - actualFrom
      wallCoords = map (\n -> move n Up lowerLeft) [actualFrom..actualTo]
      nextR = move countMax Up lowerLeft
  mapM_ (\pos -> drawChar '|' pos colors) wallCoords
  if nChars <= 0
    then
      return (nextR, from - countMax, to - countMax)
    else
      return (nextR, from + nChars - countMax, to - countMax)

-- 0 is upper left
{-# INLINABLE renderUpperWall #-}
renderUpperWall :: (Draw e) => WorldSize -> LayeredColor -> (Coords, Int, Int) -> ReaderT e IO (Coords, Int, Int)
renderUpperWall sz colors (upperLeft, from, to) = do
  let countMax = countWorldFrameHorizontal sz
      (actualFrom, actualTo) = actualRange countMax (from, to)
      nChars = 1 + actualTo - actualFrom
      nextR = translateInDir Down $ move (countMax - 1) RIGHT upperLeft
  if nChars <= 0
    then
      return (nextR, from - countMax, to - countMax)
    else
      drawChars nChars '_' (move actualFrom RIGHT upperLeft) colors
       >> return (nextR, from + nChars - countMax, to - countMax)

{-# INLINABLE renderLowerWall #-}
renderLowerWall :: (Draw e) => WorldSize -> LayeredColor -> (Coords, Int, Int) -> ReaderT e IO (Coords, Int, Int)
renderLowerWall sz colors (lowerRight, from, to) = do
  let countMax = countWorldFrameHorizontal sz
      (actualFrom, actualTo) = actualRange countMax (from, to)
      nChars = 1 + actualTo - actualFrom
      nextR = translateInDir Up $ move (countMax - 1) LEFT lowerRight
  if nChars <= 0
    then
      return (nextR, from - countMax, to - countMax)
    else
      drawChars nChars 'T' (move actualTo LEFT lowerRight) colors
       >> return (nextR, from + nChars - countMax, to - countMax)


actualRange :: Int -> (Int, Int) -> (Int, Int)
actualRange countMax (from, to) =
  (max 0 from, min to $ pred countMax)
