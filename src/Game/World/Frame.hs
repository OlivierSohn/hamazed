{-# LANGUAGE NoImplicitPrelude #-}

module Game.World.Frame
    ( mkWorldAnimation
    , mkTextAnimLeft
    , renderWorldFrame
    , -- Reexports
    module Game.World.Types
    ) where

import           Imajuscule.Prelude

import           Animation.Types

import           Color

import           Game.World.Types

import           Render
import           Render.Console

import           Text.ColorString

import           Timing

renderWorldFrame :: Evolution FrameAnimationParallel4 -> Frame -> IO ()
renderWorldFrame e frame = do
  fg <- setRawForeground worldFrameColor
  evolveIO e frame
  restoreForeground fg


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

createInterpolations :: TextAnimSpec
                     -> TextAnimSpec
                     -- ^ Upper text, Lower text, Left text 1, Left text 2
                     -> Float
                     -> (TextAnimation AnchorChars, TextAnimation AnchorStrings)
createInterpolations (TextAnimSpec [upFrom, downFrom, left1From, left2From] from)
                     (TextAnimSpec [upTo, downTo, left1To, left2To] to)
                     duration =
    let ta1 = mkTextAnimUpDown from (upFrom, downFrom) to (upTo, downTo) duration
        ta2 = mkTextAnimLeft from (left1From, left2From) to (left1To, left2To) duration
    in (ta1, ta2)
createInterpolations _ _ _ = error "not supposed to happen"


mkTextAnimLeft :: FrameSpec
               -> (ColorString, ColorString)
               -> FrameSpec
               -> (ColorString, ColorString)
               -> Float
               -> TextAnimation AnchorStrings
mkTextAnimLeft from (txtLeft1From, txtLeft2From)
               to (txtLeft1To, txtLeft2To)
               duration =
    let (_, _, leftMiddleFrom) = computeRSForInfos from
        (_, _, leftMiddleTo) = computeRSForInfos to

        rightAlignLeft = alignTxt RightAligned

        rightAlignLeft2 x = move 2 Down . rightAlignLeft x

        leftMiddle1FromAligned = rightAlignLeft txtLeft1From leftMiddleFrom
        leftMiddle1ToAligned = rightAlignLeft txtLeft1To leftMiddleTo

        leftMiddle2FromAligned = rightAlignLeft2 txtLeft2From leftMiddleFrom
        leftMiddle2ToAligned = rightAlignLeft2 txtLeft2To leftMiddleTo

    in  mkSequentialTextTranslationsStringAnchored
          [(txtLeft1From, txtLeft1To, leftMiddle1FromAligned, leftMiddle1ToAligned),
           (txtLeft2From, txtLeft2To, leftMiddle2FromAligned, leftMiddle2ToAligned)]
          duration

mkTextAnimUpDown :: FrameSpec
                 -> (ColorString, ColorString)
                 -> FrameSpec
                 -> (ColorString, ColorString)
                 -> Float
                 -> TextAnimation AnchorChars
mkTextAnimUpDown from (txtUpperFrom, txtLowerFrom)
                 to (txtUpperTo, txtLowerTo)
                 duration =
    let (centerUpFrom, centerDownFrom, _) = computeRSForInfos from
        (centerUpTo, centerDownTo, _) = computeRSForInfos to

        alignTxtCentered = alignTxt Centered

        centerUpFromAligned = alignTxtCentered txtUpperFrom centerUpFrom
        centerUpToAligned = alignTxtCentered txtUpperTo centerUpTo

        centerDownFromAligned = alignTxtCentered txtLowerFrom centerDownFrom
        centerDownToAligned = alignTxtCentered txtLowerTo centerDownTo

    in  mkSequentialTextTranslationsCharAnchored
          [(txtUpperFrom, txtUpperTo, centerUpFromAligned, centerUpToAligned),
           (txtLowerFrom, txtLowerTo, centerDownFromAligned, centerDownToAligned)]
          duration

alignTxt :: Alignment -> ColorString -> RenderState -> RenderState
alignTxt al txt = uncurry move $ align al $ countChars txt

mkWorldAnimation :: (FrameSpec, ((ColorString, ColorString), (ColorString, ColorString)))
                 -> (FrameSpec, ((ColorString, ColorString), (ColorString, ColorString)))
                 -> UTCTime
                 -- ^ time at which the animation starts
                 -> WorldAnimation
mkWorldAnimation (from, ((f1,f2),(f3,f4))) (to, ((t1,t2),(t3,t4))) t =
  WorldAnimation (WorldEvolutions frameE ta1 ta2) deadline (Iteration (Speed 1, zeroFrame))
 where
  frameE = mkEvolution (FrameAnimationParallel4 from) (FrameAnimationParallel4 to) 1
  taFrom = TextAnimSpec [f1,f2,f3,f4] from
  taTo = TextAnimSpec [t1,t2,t3,t4] to
  (ta1,ta2) = createInterpolations taFrom taTo 1
  deadline = Just $ KeyTime t
