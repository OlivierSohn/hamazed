{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Level.Animation
           (-- * Animated UI
            -- | UI elements can be animated using 'UIEvolutions':
             UIEvolutions(..)
           , mkUIAnimation
           , UIAnimation(..)
           , getDeltaTime
           , renderUIAnimation
           , isFinished
           , mkTextAnimLeft
           ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import           Imj.Draw
import           Imj.Game.World.Types
import           Imj.Geo.Discrete
import           Imj.Text.Alignment
import           Imj.Text.ColorString
import           Imj.Timing


{-# INLINABLE renderUIAnimation #-}
renderUIAnimation :: (Draw e, MonadReader e m, MonadIO m)
                  => UIAnimation
                  -> m ()
renderUIAnimation (UIAnimation we@(UIEvolutions frameE upDown left) _ (Iteration _ frame)) = do
  let (relFrameFrameE, relFrameUD, relFrameLeft) = getRelativeFrames we frame
  drawValueAt frameE relFrameFrameE
  renderAnimatedTextCharAnchored upDown relFrameUD
  renderAnimatedTextStringAnchored left relFrameLeft

-- | Compute the time interval between the current frame and the next.
getDeltaTime :: UIEvolutions -> Frame -> Maybe Float
getDeltaTime we@(UIEvolutions frameE (TextAnimation _ _ (EaseClock upDown)) (TextAnimation _ _ (EaseClock left))) frame =
  let (relFrameFrameE, relFrameUD, relFrameLeft) = getRelativeFrames we frame
  in getDeltaTimeToNextFrame frameE relFrameFrameE
    <|> getDeltaTimeToNextFrame upDown relFrameUD -- todo in TextAnimation we should have a fake evolution just for timing
    <|> getDeltaTimeToNextFrame left relFrameLeft

getRelativeFrames :: UIEvolutions
                  -> Frame
                  -> (Frame, Frame, Frame)
getRelativeFrames
 (UIEvolutions (Evolution _ lastFrameE _ _)
               (TextAnimation _ _ (EaseClock (Evolution _ lastFrameUD _ _))) _) frame =
  let relFrameRectFrameEvol = max 0 frame
      relFrameUD   = max 0 (relFrameRectFrameEvol - lastFrameE)
      relFrameLeft = max 0 (relFrameUD - lastFrameUD)
  in (relFrameRectFrameEvol, relFrameUD, relFrameLeft)


mkUIAnimation :: (RectFrame, (([ColorString], [ColorString]), [[ColorString]]))
              -- ^ From
              -> (RectFrame, (([ColorString], [ColorString]), [[ColorString]]))
              -- ^ To
              -> SystemTime
              -- ^ Time at which the animation starts
              -> UIAnimation
mkUIAnimation (from, ((f1,f2),f3)) (to, ((t1,t2),t3)) t =
  UIAnimation evolutions deadline (Iteration (Speed 1) zeroFrame)
 where
  frameE = mkEvolution (Successive [from, to]) 1

  (ta1,ta2) = createUITextAnimations from to (f1++t1, f2++t2, zipWith (++) f3 t3) 1
  evolutions = UIEvolutions frameE ta1 ta2
  deadline =
    maybe
      Nothing
      (\dt -> Just $ KeyTime $ addSystemTime (floatSecondsToDiffTime dt) t)
      $ getDeltaTime evolutions zeroFrame


createUITextAnimations :: RectFrame
                       -- ^ From
                       -> RectFrame
                       -- ^ To
                       -> ([ColorString],[ColorString],[[ColorString]])
                       -- ^ Upper text, Lower text, Left texts
                       -> Float
                       -> (TextAnimation AnchorChars, TextAnimation AnchorStrings)
createUITextAnimations from to (ups, downs, lefts) duration =
    let ta1 = mkTextAnimUpDown from to (ups, downs) duration
        ta2 = mkTextAnimLeft from to lefts duration
    in (ta1, ta2)

-- | Creates the 'TextAnimation' to animate the texts that appears left of the main
-- 'RectFrame'

mkTextAnimLeft :: RectFrame
               -- ^ From 'RectFrame'
               -> RectFrame
               -- ^ To 'RectFrame'
               -> [[ColorString]]
                -- ^ Each inner list is expected to be of length 1 or more.
                --
                -- If length = 1, the 'ColorString' is not animated. Else, the inner list
                -- contains 'ColorString' waypoints.
               -> Float
               -- ^ The duration of the animation
               -> TextAnimation AnchorStrings
mkTextAnimLeft from to listTxts
               duration =
    let (_, _, leftMiddleFrom) = computeRSForInfos from
        (_, _, leftMiddleTo) = computeRSForInfos to

        l = zipWith (\i txts ->
                      let firstTxt = head txts
                          lastTxt = last txts
                          rightAlign pos = move (2*i) Down . alignTxt (mkRightAlign pos)
                          fromAligned = rightAlign leftMiddleFrom firstTxt
                          toAligned   = rightAlign leftMiddleTo lastTxt
                      in (txts, fromAligned, toAligned))
                    [0..] listTxts

    in  mkSequentialTextTranslationsStringAnchored l duration

mkTextAnimUpDown :: RectFrame
                 -> RectFrame
                 -> ([ColorString], [ColorString])
                 -- ^ Each list is expected to be of size at least 1.
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


computeRSForInfos :: RectFrame -> (Coords, Coords, Coords)
computeRSForInfos (RectFrame (Size rs cs) upperLeft _) =
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
