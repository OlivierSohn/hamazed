{-# LANGUAGE NoImplicitPrelude #-}

module Game.World.Evolution
           ( mkTextAnimLeft
           , getDeltaTime
           , renderEvolutions
           , mkWorldAnimation
           ) where

import           Imajuscule.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import           Game.World.Types

import           Geo.Discrete

import           Draw

import           Text.Alignment
import           Text.ColorString

import           Timing

{-# INLINABLE renderEvolutions #-}
renderEvolutions :: (Draw e, MonadReader e m, MonadIO m)
                 => WorldEvolutions
                 -> Frame
                 -> m ()
renderEvolutions we@(WorldEvolutions frameE upDown left) frame  = do
  let (relFrameFrameE, relFrameUD, relFrameLeft) = getFrames we frame
  drawValueAt frameE relFrameFrameE
  renderAnimatedTextCharAnchored upDown relFrameUD
  renderAnimatedTextStringAnchored left relFrameLeft

getDeltaTime :: WorldEvolutions -> Frame -> Maybe Float
getDeltaTime we@(WorldEvolutions frameE (TextAnimation _ _ (EaseClock upDown)) (TextAnimation _ _ (EaseClock left))) frame =
  let (relFrameFrameE, relFrameUD, relFrameLeft) = getFrames we frame
  in getDeltaTimeToNextFrame frameE relFrameFrameE
    <|> getDeltaTimeToNextFrame upDown relFrameUD -- todo in TextAnimation we should have a fake evolution just for timing
    <|> getDeltaTimeToNextFrame left relFrameLeft

getFrames :: WorldEvolutions -> Frame -> (Frame, Frame, Frame)
getFrames (WorldEvolutions (Evolution _ lastFrameFrameE _ _)
                           (TextAnimation _ _ (EaseClock (Evolution _ lastFrameUD _ _))) _) frame =
  let relFrameFrameE = max 0 frame
      relFrameUD = max 0 (relFrameFrameE - lastFrameFrameE)
      relFrameLeft = max 0 (relFrameUD - lastFrameUD)
  in (relFrameFrameE, relFrameUD, relFrameLeft)


mkWorldAnimation :: (FrameSpec, (([ColorString], [ColorString]), ([ColorString], [ColorString])))
                 -> (FrameSpec, (([ColorString], [ColorString]), ([ColorString], [ColorString])))
                 -> UTCTime
                 -- ^ Time at which the animation starts
                 -> WorldAnimation
mkWorldAnimation (from, ((f1,f2),(f3,f4))) (to, ((t1,t2),(t3,t4))) t =
  WorldAnimation evolutions deadline (Iteration (Speed 1) zeroFrame)
 where
  frameE = mkEvolution (Successive [FrameAnimationParallel4 from, FrameAnimationParallel4 to]) 1
  (ta1,ta2) = createInterpolations from to (f1++t1, f2++t2, f3++t3, f4++t4) 1
  evolutions = WorldEvolutions frameE ta1 ta2
  deadline =
    maybe
      Nothing
      (\dt -> Just $ KeyTime $ addUTCTime (floatSecondsToNominalDiffTime dt) t)
      $ getDeltaTime evolutions zeroFrame


createInterpolations :: FrameSpec
                     -> FrameSpec
                     -> ([ColorString],[ColorString],[ColorString],[ColorString])
                     -- ^ Upper text, Lower text, Left text 1, Left text 2
                     -> Float
                     -> (TextAnimation AnchorChars, TextAnimation AnchorStrings)
createInterpolations from to (ups, downs, left1s, left2s) duration =
    let ta1 = mkTextAnimUpDown from to (ups, downs) duration
        ta2 = mkTextAnimLeft from to (left1s, left2s) duration
    in (ta1, ta2)


mkTextAnimLeft :: FrameSpec
               -> FrameSpec
               -> ([ColorString], [ColorString])
               -> Float
               -> TextAnimation AnchorStrings
mkTextAnimLeft from to (txtLeft1s, txtLeft2s)
               duration =
    let (_, _, leftMiddleFrom) = computeRSForInfos from
        (_, _, leftMiddleTo) = computeRSForInfos to

        rightAlignLeft pos = alignTxt $ mkRightAlign pos

        rightAlignLeft2 x = move 2 Down . rightAlignLeft x

        leftMiddle1FromAligned = rightAlignLeft leftMiddleFrom (head txtLeft1s)
        leftMiddle1ToAligned   = rightAlignLeft leftMiddleTo (last txtLeft1s)

        leftMiddle2FromAligned = rightAlignLeft2 leftMiddleFrom (head txtLeft2s)
        leftMiddle2ToAligned   = rightAlignLeft2 leftMiddleTo (last txtLeft2s)

    in  mkSequentialTextTranslationsStringAnchored
          [(txtLeft1s, leftMiddle1FromAligned, leftMiddle1ToAligned),
           (txtLeft2s, leftMiddle2FromAligned, leftMiddle2ToAligned)]
          duration

mkTextAnimUpDown :: FrameSpec
                 -> FrameSpec
                 -> ([ColorString], [ColorString])
                 -> Float
                 -> TextAnimation AnchorChars
mkTextAnimUpDown from to (txtUppers, txtLowers)
                 duration =
    let (centerUpFrom, centerDownFrom, _) = computeRSForInfos from
        (centerUpTo, centerDownTo, _) = computeRSForInfos to

        alignTxtCentered pos = alignTxt $ mkCentered pos

        centerUpFromAligned = alignTxtCentered centerUpFrom (head txtUppers)
        centerUpToAligned   = alignTxtCentered centerUpTo (last txtUppers)

        centerDownFromAligned = alignTxtCentered centerDownFrom (head txtLowers)
        centerDownToAligned   = alignTxtCentered centerDownTo (last txtLowers)
    in  mkSequentialTextTranslationsCharAnchored
          [(txtUppers, centerUpFromAligned, centerUpToAligned),
           (txtLowers, centerDownFromAligned, centerDownToAligned)]
          duration

alignTxt :: Alignment -> ColorString  -> Coords
alignTxt (Alignment al pos) txt =
  uncurry move (align al $ countChars txt) pos


computeRSForInfos :: FrameSpec -> (Coords, Coords, Coords)
computeRSForInfos (FrameSpec (Size rs cs) upperLeft _) =
  (centerUp, centerDown, leftMiddle)
 where
  addWallSize = (+ 2)
  half = flip quot 2
  mkSizes s = (addWallSize s, half s)
  (rFull, rHalf) = mkSizes rs
  cHalf = quot cs 2

  centerUp   = translate' (-1)        (cHalf + 1) upperLeft
  centerDown = translate' (rFull + 1) (cHalf + 1) upperLeft
  leftMiddle = translate' (rHalf + 1) (-1)  upperLeft
