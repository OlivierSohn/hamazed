{-# LANGUAGE NoImplicitPrelude #-}

module Imj.UI.Animation
           (-- * Animated UI
             UIEvolutions(..)
           , mkUIAnimation
           , UIAnimation(..)
           , getDeltaTime
           , renderUIAnimation
           , isFinished
           , mkTextAnimRightAligned
           ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import           Imj.Draw
import           Imj.Geo.Discrete
import           Imj.Iteration
import           Imj.Text.Alignment
import           Imj.Text.Animation
import           Imj.Text.ColorString
import           Imj.Timing
import           Imj.UI.RectContainer


-- | Manages the progress and deadline of 'UIEvolutions'.
data UIAnimation = UIAnimation {
    _uiAnimationEvs :: !UIEvolutions
  , _uiAnimationDeadline :: !(Maybe KeyTime)
  -- ^ Time at which the 'UIEvolutions' should be rendered and updated
  , _uiAnimationProgress :: !Iteration
  -- ^ Current 'Iteration'.
} deriving(Show)


-- TODO generalize as an Evolution (text-decorated RectContainer)
-- | Used when transitionning between two levels to smoothly transform the aspect
-- of the 'RectContainer', as well as textual information around it.
data UIEvolutions = UIEvolutions {
    _uiEvolutionContainer :: !(Evolution RectContainer)
    -- ^ The transformation of the 'RectContainer'.
  , _uiEvolutionsUpDown :: !(TextAnimation AnchorChars)
    -- ^ The transformation of colored text at the top and at the bottom of the 'RectContainer'.
  , _uiEvolutionLeft    :: !(TextAnimation AnchorStrings)
    -- ^ The transformation of colored text left and right of the 'RectContainer'.
} deriving(Show)



-- | Is the 'UIAnimation' finished?
isFinished :: UIAnimation -> Bool
isFinished (UIAnimation _ Nothing _) = True
isFinished _ = False

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


mkUIAnimation :: (RectContainer, (([ColorString], [ColorString]), [[ColorString]]))
              -- ^ From
              -> (RectContainer, (([ColorString], [ColorString]), [[ColorString]]))
              -- ^ To
              -> SystemTime
              -- ^ Time at which the animation starts
              -> UIAnimation
mkUIAnimation (from, ((f1,f2),f3)) (to, ((t1,t2),t3)) t =
  UIAnimation evolutions deadline (Iteration (Speed 1) zeroFrame)
 where
  frameE = mkEvolutionEaseQuart (Successive [from, to]) 1

  (ta1,ta2) = createUITextAnimations from to (f1++t1, f2++t2, zipWith (++) f3 t3) 1
  evolutions = UIEvolutions frameE ta1 ta2
  deadline =
    maybe
      Nothing
      (\dt -> Just $ KeyTime $ addSystemTime (floatSecondsToDiffTime dt) t)
      $ getDeltaTime evolutions zeroFrame


createUITextAnimations :: RectContainer
                       -- ^ From
                       -> RectContainer
                       -- ^ To
                       -> ([ColorString],[ColorString],[[ColorString]])
                       -- ^ Upper text, Lower text, Left texts
                       -> Float
                       -> (TextAnimation AnchorChars, TextAnimation AnchorStrings)
createUITextAnimations from to (ups, downs, lefts) duration =
    let (centerUpFrom, centerDownFrom, leftMiddleFrom) = getSideCentersAtDistance from 2
        (centerUpTo, centerDownTo, leftMiddleTo) = getSideCentersAtDistance to 2
        ta1 = mkTextAnimCenteredUpDown (centerUpFrom, centerDownFrom) (centerUpTo, centerDownTo) (ups, downs) duration
        ta2 = mkTextAnimRightAligned leftMiddleFrom leftMiddleTo lefts duration
    in (ta1, ta2)

-- | Creates the 'TextAnimation' to animate the texts that appears left of the main
-- 'RectContainer'

mkTextAnimRightAligned :: Coords
                       -- ^ Alignment ref /from/
                       -> Coords
                       -- ^ Alignment ref /to/
                       -> [[ColorString]]
                       -- ^ Each inner list is expected to be of length 1 or more.
                       --
                       -- If length = 1, the 'ColorString' is not animated. Else, the inner list
                       -- contains 'ColorString' waypoints.
                       -> Float
                       -- ^ The duration of the animation
                       -> TextAnimation AnchorStrings
mkTextAnimRightAligned refFrom refTo listTxts duration =
  let l = zipWith (\i txts ->
                    let firstTxt = head txts
                        lastTxt = last txts
                        rightAlign pos = move (2*i) Down . alignTxt (mkRightAlign pos)
                        fromAligned = rightAlign refFrom firstTxt
                        toAligned   = rightAlign refTo lastTxt
                    in (txts, fromAligned, toAligned))
                  [0..] listTxts
  in  mkSequentialTextTranslationsStringAnchored l duration

mkTextAnimCenteredUpDown :: (Coords, Coords)
                         -> (Coords, Coords)
                         -> ([ColorString], [ColorString])
                         -- ^ Each list is expected to be of size at least 1.
                         -> Float
                         -> TextAnimation AnchorChars
mkTextAnimCenteredUpDown (centerUpFrom, centerDownFrom) (centerUpTo, centerDownTo) (txtUppers, txtLowers)
                 duration =
    let alignTxtCentered pos = alignTxt $ mkCentered pos

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
