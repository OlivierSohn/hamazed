{-# OPTIONS_HADDOCK hide #-} -- TODO refactor and doc

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.UI.Animation
           ( mkUIAnimation
           , getDeltaTime
           , drawUIAnimation
           , mkTextAnimRightAligned
           -- reexports
           , module Imj.Graphics.UI.Animation.Types
           ) where

import           Imj.Prelude hiding(lefts)
import qualified Prelude as Unsafe(head,last)

import           Data.List(length)

import           Imj.Geo.Discrete.Types
import           Imj.Graphics.UI.Animation.Types
import           Imj.Graphics.Text.Animation.Types

import           Imj.Graphics.Class.DiscreteInterpolation
import           Imj.Graphics.Class.Words(Characters)
import qualified Imj.Graphics.Class.Words as Words
import           Imj.Graphics.Render
import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.Text.Animation
import           Imj.Graphics.Text.ColoredGlyphList
import           Imj.Graphics.UI.Colored
import           Imj.Graphics.UI.RectContainer
import           Imj.Timing

{-# INLINABLE drawUIAnimation #-}
drawUIAnimation :: (Draw e, MonadReader e m, MonadIO m)
                => UIAnimation
                -> m ()
drawUIAnimation
 (UIAnimation we@(UIEvolutions containerEvolution upDown left) (UIAnimProgress _ (Iteration _ frame))) = do
  let (relFrameFrameE, relFrameUD, relFrameLeft) = getRelativeFrames we frame
  drawMorphingAt containerEvolution relFrameFrameE
  drawAnimatedTextCharAnchored upDown relFrameUD
  drawAnimatedTextStringAnchored left relFrameLeft

-- | Compute the time interval between the current frame and the next.
getDeltaTime :: UIEvolutions -> Frame -> Maybe (Time Duration System)
getDeltaTime we@(UIEvolutions containerEvolution (TextAnimation _ _ (EaseClock upDown)) (TextAnimation _ _ (EaseClock left))) frame =
  let (relFrameFrameE, relFrameUD, relFrameLeft) = getRelativeFrames we frame
  in getDeltaTimeToNextFrame containerEvolution relFrameFrameE
    <|> getDeltaTimeToNextFrame upDown relFrameUD -- todo in TextAnimation we should have a fake evolution just for timing
    <|> getDeltaTimeToNextFrame left relFrameLeft

getRelativeFrames :: UIEvolutions
                  -> Frame
                  -> (Frame, Frame, Frame)
getRelativeFrames
 (UIEvolutions (Evolution _ lastFrameContainerEvolution _ _)
               (TextAnimation _ _ (EaseClock (Evolution _ lastFrameUD _ _))) _) frame =
  let relFrameRectFrameEvol = max 0 frame
      relFrameUD   = max 0 (relFrameRectFrameEvol - lastFrameContainerEvolution)
      relFrameLeft = max 0 (relFrameUD - lastFrameUD)
  in (relFrameRectFrameEvol, relFrameUD, relFrameLeft)

mkUIAnimation :: (Colored RectContainer, ((Successive ColoredGlyphList, Successive ColoredGlyphList), [Successive ColoredGlyphList]))
              -- ^ From
              -> (Colored RectContainer, ((Successive ColoredGlyphList, Successive ColoredGlyphList), [Successive ColoredGlyphList]))
              -- ^ To
              -> Length Width
              -> Length Height
              -> Time Point System
              -- ^ Time at which the animation starts
              -> UIAnimation
mkUIAnimation (from@(Colored _ fromR@(RectContainer (Size fh fw) _)), ((f1,f2),f3))
              (to@(Colored _ toR@(RectContainer (Size th tw) _)), ((t1,t2),t3))
              horizontalDistance verticalDistance time =
  UIAnimation evolutions $ UIAnimProgress deadline $ Iteration (Speed 1) zeroFrame

 where

  dx = max (abs $ fromIntegral fw - fromIntegral tw)
           (abs $ fromIntegral fh - fromIntegral th)

  duration = fromSecs $ 1 + max 0 (dx - 2) / 80 -- slow down if distances are bigger

  frameE = mkEvolutionEaseQuart (Successive [from, to]) duration

  (ta1,ta2) = createUITextAnimations fromR toR (concatSuccessive f1 t1,
                                                concatSuccessive f2 t2,
                                                zipWith concatSuccessive
                                                  (f3 ++ repeat (Successive []))
                                                  t3)
                                     horizontalDistance verticalDistance duration

  evolutions = UIEvolutions frameE ta1 ta2

  deadline =
    maybe
      Nothing
      (\dt -> Just $ addDuration dt time)
      $ getDeltaTime evolutions zeroFrame

createUITextAnimations :: (Characters a, DiscreteDistance a)
                       => RectContainer
                       -- ^ From
                       -> RectContainer
                       -- ^ To
                       -> (Successive a,
                           Successive a,
                           [Successive a])
                       -- ^ Upper text, Lower text, Left texts
                       -> Length Width
                       -> Length Height
                       -> Time Duration System
                       -> (TextAnimation a AnchorChars,
                           TextAnimation a AnchorStrings)
createUITextAnimations from to (ups, downs, lefts) horizontalDistance verticalDistance duration =
  let (centerUpFrom, centerDownFrom, leftMiddleFrom, _) =
        getSideCenters $ mkRectContainerAtDistance from horizontalDistance verticalDistance
      (centerUpTo, centerDownTo, leftMiddleTo, _) =
        getSideCenters $ mkRectContainerAtDistance to horizontalDistance verticalDistance
      ta1 = mkTextAnimCenteredUpDown (centerUpFrom, centerDownFrom) (centerUpTo, centerDownTo) (ups, downs) duration
      ta2 = mkTextAnimRightAligned leftMiddleFrom leftMiddleTo lefts 1 duration
  in (ta1, ta2)

-- | Creates the 'TextAnimation' to animate the texts that appears left of the main
-- 'RectContainer'.
--
-- Text will be horizontally right-aligned and vertically centered according to
-- alignment coordinates.
mkTextAnimRightAligned :: (Characters a, DiscreteDistance a)
                       => Coords Pos
                       -- ^ Alignment ref /from/.
                       -> Coords Pos
                       -- ^ Alignment ref /to/ Text will be vertically centered
                       -- according to it.
                       -> [Successive a]
                       -- ^ Each 'Successive' is expected to be of length 1 or more.
                       -> Int
                       -- ^ Interline spaces
                       -> Time Duration System
                       -- ^ The duration of the animation
                       -> TextAnimation a AnchorStrings
mkTextAnimRightAligned refFrom refTo listTxts interline duration =
  let dy = 1+interline -- move that amount for a new line
      nTxtLines = length listTxts
      heightTxt = nTxtLines + pred nTxtLines * interline
      verticalOffset = quot heightTxt 2
      l = zipWith (\i s@(Successive txts) ->
        case txts of
          [] -> error "logic"
          _:_ ->
            let firstTxt = Unsafe.head txts
                lastTxt = Unsafe.last txts
                rightAlign pos =
                  move verticalOffset Up .
                  move (dy*i) Down .
                  alignTxt (mkRightAlign pos)
                fromAligned = rightAlign refFrom firstTxt
                toAligned   = rightAlign refTo lastTxt
            in (s, fromAligned, toAligned))
          [0..] listTxts
  in  mkSequentialTextTranslationsStringAnchored l duration

mkTextAnimCenteredUpDown :: (Characters a, DiscreteDistance a)
                         => (Coords Pos, Coords Pos)
                         -> (Coords Pos, Coords Pos)
                         -> (Successive a, Successive a)
                         -- ^ If one 'Successive' has a 0 length, the animation will be empty.
                         -> Time Duration System
                         -> TextAnimation a AnchorChars
mkTextAnimCenteredUpDown _ _ (Successive [], _) _ = mkEmptyTextAnimation
mkTextAnimCenteredUpDown _ _ (_, Successive []) _ = mkEmptyTextAnimation
mkTextAnimCenteredUpDown (centerUpFrom, centerDownFrom) (centerUpTo, centerDownTo) (sUp@(Successive txtUppers@(_:_)), sLow@(Successive txtLowers@(_:_))) duration =
  let alignTxtCentered pos = alignTxt $ mkCentered pos

      centerUpFromAligned = alignTxtCentered centerUpFrom (Unsafe.head txtUppers)
      centerUpToAligned   = alignTxtCentered centerUpTo (Unsafe.last txtUppers)

      centerDownFromAligned = alignTxtCentered centerDownFrom (Unsafe.head txtLowers)
      centerDownToAligned   = alignTxtCentered centerDownTo (Unsafe.last txtLowers)
  in mkSequentialTextTranslationsCharAnchored
      [(sUp, centerUpFromAligned, centerUpToAligned),
       (sLow, centerDownFromAligned, centerDownToAligned)]
      duration

alignTxt :: (Characters a) => Alignment -> a -> Coords Pos
alignTxt (Alignment al pos) txt =
  uncurry move (align al $ fromIntegral $ Words.length txt) pos
