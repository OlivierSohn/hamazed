{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Game.World.Frame
    ( renderWorldFrame
    , maxNumberOfSteps
    , computeRSForInfos
    , mkFrameAnimation
    , mkWorldAnimation
    , createInterpolations
    , -- Reexports
    module Game.World.Types
    ) where

import           Imajuscule.Prelude

import           Animation.Types

import           Data.List( mapAccumL, zip )

import           Color

import           Game.World.Types
import           Game.World.Size

import           Interpolation

import           Render
import           Render.Console

import           Timing

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

renderWorldFrame :: WorldAnimation -- ^ contains next world
                 -> World -- ^ current world
                 -> IO ()
renderWorldFrame (WorldAnimation (FrameAnimation (World _ _ _ (Space _ szNext _) _ (EmbeddedWorld _ nextUpperLeft)) _ _ _ lastFAFrame)
                                 _ _ (Iteration (_, frame@(Frame i))))
                  (World _ _ _ (Space _ szCur _) _ (EmbeddedWorld _ curUpperLeft)) = do
  fg <- setRawForeground worldFrameColor
  let from = FrameSpec szCur curUpperLeft
      to = FrameSpec szNext  nextUpperLeft
  if frame < lastFAFrame
    then
      renderTransition from to i
    else
      renderWhole to
  restoreForeground fg

renderWhole :: FrameSpec -> IO ()
renderWhole (FrameSpec sz upperLeft) =
  renderPartialWorldFrame sz (upperLeft, 0, countWorldFrameChars sz - 1)

renderTransition :: FrameSpec -> FrameSpec -> Int -> IO ()
renderTransition from@(FrameSpec fromSz fromUpperLeft) to@(FrameSpec toSz toUpperLeft) i = do
      let
          (RenderState (Coords _ (Col dc))) = diffRS fromUpperLeft toUpperLeft
          n = maxNumberOfSteps fromSz toSz
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

-- | Includes start and end steps, ie if animation consists of no change, it returns 1.
--   If animation consists of a single change, it returns 2.
maxNumberOfSteps :: WorldSize -> WorldSize -> Int
maxNumberOfSteps s s' = 1 + quot (1 + max (maxDim s) (maxDim s')) 2

data BuildFrom = Middle
               | Extremities -- generates the complement

ranges :: Int -> WorldSize -> BuildFrom -> [(Int, Int)]
ranges progress sz =
  let h = countWorlFrameVertical sz
      w = countWorlFrameHorizontal sz

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


computeRSForInfos :: FrameSpec -> (RenderState, RenderState, RenderState)
computeRSForInfos (FrameSpec (WorldSize (Coords (Row rs) (Col cs))) upperLeft) =
  (centerUp, centerDown, leftMiddle)
 where
  addWallSize = (+ 2)
  half = flip quot 2
  mkSizes s = (addWallSize s, half s)
  (rFull, rHalf) = mkSizes rs
  (_    , cHalf) = mkSizes cs

  centerUp   = translate (Row $ -1)        (Col $ cHalf + 1) upperLeft
  centerDown = translate (Row $ rFull + 1) (Col $ cHalf + 1) upperLeft
  leftMiddle = translate (Row $ rHalf + 1) (Col $ -1)  upperLeft

createInterpolations :: FrameSpec
                     -> FrameSpec
                     -> Float
                     -> [Evolution RenderState]
createInterpolations from to duration =
    let (centerUpFrom, centerDownFrom, leftMiddleFrom) = computeRSForInfos from
        (centerUpTo, centerDownTo, leftMiddleTo) = computeRSForInfos to
        evol f t = mkEvolution f t duration
    in [evol centerUpFrom centerUpTo,
        evol centerDownFrom centerDownTo,
        evol leftMiddleFrom leftMiddleTo]


mkWorldAnimation :: World
                 -> World
                 -> UTCTime
                 -- ^ time at which the animation starts
                 -> FrameAnimation
                 -> WorldAnimation
mkWorldAnimation
  (World _ _ _ (Space _ fromSz _) _ (EmbeddedWorld _ fromUpperLeft))
  (World _ _ _ (Space _ toSz _) _ (EmbeddedWorld _ toUpperLeft))
  t fa =
  WorldAnimation fa (createInterpolations from to 0.3) deadline (Iteration (Speed 1, startFrame))
 where
  startFrame = Frame (-1)
  from = FrameSpec fromSz fromUpperLeft
  to = FrameSpec toSz toUpperLeft
  deadline =
    if from == to
      then Nothing
      else Just $ KeyTime t

mkFrameAnimation :: World
                 -- ^ next world
                 -> Float
                 -- ^ duration
                 -> (Float -> Float)
                 -- ^ inverse ease function
                 -> Frame
                 -- ^ last frame
                 -> FrameAnimation
mkFrameAnimation next =
  FrameAnimation next Nothing
