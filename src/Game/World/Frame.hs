{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Game.World.Frame
    ( maxNumberOfSteps
    , mkFrameAnimation
    , mkWorldAnimation
    , mkTextAnimLeft
    , renderWorldFrame
    , -- Reexports
    module Game.World.Types
    ) where

import           Imajuscule.Prelude

import           Animation.Types

import           Color

import           Data.List( mapAccumL, zip )

import           Game.World.Types
import           Game.World.Size

import           Render
import           Render.Console

import           Text.ColorString

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
  let countMax = countWorlFrameVertical sz
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
  let countMax = countWorlFrameHorizontal sz
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
  let countMax = countWorlFrameHorizontal sz
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

renderWorldFrame :: World -> World -> Frame -> Frame -> IO ()
renderWorldFrame
 (World _ _ _ (Space _ szCur _) _ (EmbeddedWorld _ curUpperLeft))
 (World _ _ _ (Space _ szNext _) _ (EmbeddedWorld _ nextUpperLeft))
 frame@(Frame i) lastFAFrame = do
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
                     -> ((ColorString, ColorString), (ColorString, ColorString))
                     -- ^ Upper text, Lower text, Left text 1, Left text 2
                     -> Float
                     -> WorldEvolutions
createInterpolations from to ((txtUpper, txtLower), (txtLeft1, txtLeft2)) duration =
    let ta1 = mkTextAnimUpDown from to (txtUpper, txtLower) duration
        ta2 = mkTextAnimLeft from to (txtLeft1, txtLeft2) duration
    in WorldEvolutions ta1 ta2


mkTextAnimLeft :: FrameSpec
               -> FrameSpec
               -> (ColorString, ColorString)
               -> Float
               -> TextAnimation
mkTextAnimLeft from to (txtLeft1, txtLeft2) duration =
    let (_, _, leftMiddleFrom) = computeRSForInfos from
        (_, _, leftMiddleTo) = computeRSForInfos to

        rightAlignLeft = alignTxt RightAligned

        rightAlignLeft1 = rightAlignLeft txtLeft1
        rightAlignLeft2 = move 2 Down . rightAlignLeft txtLeft2

        leftMiddle1FromAligned = rightAlignLeft1 leftMiddleFrom
        leftMiddle1ToAligned = rightAlignLeft1 leftMiddleTo

        leftMiddle2FromAligned = rightAlignLeft2 leftMiddleFrom
        leftMiddle2ToAligned = rightAlignLeft2 leftMiddleTo

    in  mkSequentialTextTranslations
          [(txtLeft1, leftMiddle1FromAligned, leftMiddle1ToAligned),
           (txtLeft2, leftMiddle2FromAligned, leftMiddle2ToAligned)]
          duration

mkTextAnimUpDown :: FrameSpec
                 -> FrameSpec
                 -> (ColorString, ColorString)
                 -> Float
                 -> TextAnimation
mkTextAnimUpDown from to (txtUpper, txtLower) duration =
    let (centerUpFrom, centerDownFrom, _) = computeRSForInfos from
        (centerUpTo, centerDownTo, _) = computeRSForInfos to

        alignTxtCentered = alignTxt Centered

        alignUpperTxtCentered = alignTxtCentered txtUpper
        alignLowerTxtCentered = alignTxtCentered txtLower

        centerUpFromAligned = alignUpperTxtCentered centerUpFrom
        centerUpToAligned = alignUpperTxtCentered centerUpTo

        centerDownFromAligned = alignLowerTxtCentered centerDownFrom
        centerDownToAligned = alignLowerTxtCentered centerDownTo

    in  mkSequentialTextTranslations
          [(txtUpper, centerUpFromAligned, centerUpToAligned),
           (txtLower, centerDownFromAligned, centerDownToAligned)]
          duration

alignTxt :: Alignment -> ColorString -> RenderState -> RenderState
alignTxt al txt = uncurry move $ align al $ countChars txt

mkWorldAnimation :: World
                 -> World
                 -> ((ColorString, ColorString), (ColorString, ColorString))
                 -> UTCTime
                 -- ^ time at which the animation starts
                 -> FrameAnimation
                 -> WorldAnimation
mkWorldAnimation wFrom wTo infos t fa =
  WorldAnimation fa (createInterpolations from to infos 1) deadline (Iteration (Speed 1, startFrame))
 where
  startFrame = pred zeroFrame
  from = mkFrameSpec wFrom
  to   = mkFrameSpec wTo
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
