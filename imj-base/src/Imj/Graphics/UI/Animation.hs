{-# OPTIONS_HADDOCK hide #-} -- TODO refactor and doc

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.UI.Animation
           (-- * Animated UI
             UIEvolutions(..)
           , mkUIAnimation
           , UIAnimation(..)
           , getDeltaTime
           , getUIAnimationDeadline
           , drawUIAnimation
           , isFinished
           , mkTextAnimRightAligned
           ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)
import           Data.List(length)

import           Imj.Geo.Discrete
import           Imj.Graphics.Render
import           Imj.Graphics.Text.Alignment
import           Imj.Graphics.Text.Animation
import           Imj.Graphics.Text.ColorString
import           Imj.Graphics.UI.Colored
import           Imj.Graphics.UI.RectContainer
import           Imj.Timing


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
    _uiEvolutionContainer :: !(Evolution (Colored RectContainer))
    -- ^ The transformation of the 'RectContainer'.
  , _uiEvolutionsUpDown :: !(TextAnimation AnchorChars)
    -- ^ The transformation of colored text at the top and at the bottom of the 'RectContainer'.
  , _uiEvolutionLeft    :: !(TextAnimation AnchorStrings)
    -- ^ The transformation of colored text left and right of the 'RectContainer'.
} deriving(Show)


getUIAnimationDeadline :: UIAnimation -> Maybe KeyTime
getUIAnimationDeadline (UIAnimation _ mayDeadline _) =
  mayDeadline

-- | Is the 'UIAnimation' finished?
isFinished :: UIAnimation -> Bool
isFinished (UIAnimation _ Nothing _) = True
isFinished _ = False

{-# INLINABLE drawUIAnimation #-}
drawUIAnimation :: (Draw e, MonadReader e m, MonadIO m)
                  => Coords Pos
                  -- ^ Offset for container
                  -> UIAnimation
                  -> m ()
drawUIAnimation containerOffset
 (UIAnimation we@(UIEvolutions containerEvolution upDown left) _ (Iteration _ frame)) = do
  let (relFrameFrameE, relFrameUD, relFrameLeft) = getRelativeFrames we frame
  drawMorphingAt (moveContainerEvolution containerEvolution containerOffset) relFrameFrameE
  drawAnimatedTextCharAnchored upDown relFrameUD
  drawAnimatedTextStringAnchored left relFrameLeft

moveContainerEvolution :: Evolution (Colored RectContainer)
                       -> Coords Pos
                       -> Evolution (Colored RectContainer)
moveContainerEvolution ev' offset =
  if zeroCoords /= offset
    then
      fmap (fmap (applyOffset offset)) ev'
    else
      ev'


-- | Compute the time interval between the current frame and the next.
getDeltaTime :: UIEvolutions -> Frame -> Maybe Float
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


mkUIAnimation :: (Colored RectContainer, (([ColorString], [ColorString]), [[ColorString]]))
              -- ^ From
              -> (Colored RectContainer, (([ColorString], [ColorString]), [[ColorString]]))
              -- ^ To
              -> Length Width
              -> Length Height
              -> SystemTime
              -- ^ Time at which the animation starts
              -> UIAnimation
mkUIAnimation (from@(Colored _ fromR), ((f1,f2),f3))
              (to@(Colored _ toR), ((t1,t2),t3))
              horizontalDistance verticalDistance t =
  UIAnimation evolutions deadline (Iteration (Speed 1) zeroFrame)
 where
  frameE = mkEvolutionEaseQuart (Successive [from, to]) 1

  (ta1,ta2) = createUITextAnimations fromR toR (f1++t1, f2++t2, zipWith (++) f3 t3)
                                     horizontalDistance verticalDistance 1
  evolutions = UIEvolutions frameE ta1 ta2
  deadline =
    maybe
      Nothing
      (\dt -> Just $ KeyTime $ addToSystemTime (floatSecondsToDiffTime dt) t)
      $ getDeltaTime evolutions zeroFrame


createUITextAnimations :: RectContainer
                       -- ^ From
                       -> RectContainer
                       -- ^ To
                       -> ([ColorString],[ColorString],[[ColorString]])
                       -- ^ Upper text, Lower text, Left texts
                       -> Length Width
                       -> Length Height
                       -> Float
                       -> (TextAnimation AnchorChars, TextAnimation AnchorStrings)
createUITextAnimations from to (ups, downs, lefts) horizontalDistance verticalDistance duration =
    let (centerUpFrom, centerDownFrom, leftMiddleFrom, _) =
          getSideCentersAtDistance from horizontalDistance verticalDistance
        (centerUpTo, centerDownTo, leftMiddleTo, _) =
          getSideCentersAtDistance to horizontalDistance verticalDistance
        ta1 = mkTextAnimCenteredUpDown (centerUpFrom, centerDownFrom) (centerUpTo, centerDownTo) (ups, downs) duration
        ta2 = mkTextAnimRightAligned leftMiddleFrom leftMiddleTo lefts 1 duration
    in (ta1, ta2)

-- | Creates the 'TextAnimation' to animate the texts that appears left of the main
-- 'RectContainer'.
--
-- Text will be horizontally right-aligned and vertically centered according to
-- alignment coordinates.
mkTextAnimRightAligned :: Coords Pos
                       -- ^ Alignment ref /from/.
                       -> Coords Pos
                       -- ^ Alignment ref /to/ Text will be vertically centered
                       -- according to it.
                       -> [[ColorString]]
                       -- ^ Each inner list is expected to be of length 1 or more.
                       --
                       -- If length = 1, the 'ColorString' is not animated. Else, the inner list
                       -- contains 'ColorString' waypoints.
                       -> Int
                       -- ^ Interline spaces
                       -> Float
                       -- ^ The duration of the animation
                       -> TextAnimation AnchorStrings
mkTextAnimRightAligned refFrom refTo listTxts interline duration =
  let dy = 1+interline -- move that amount for a new line
      nTxtLines = length listTxts
      heightTxt = nTxtLines + pred nTxtLines * interline
      verticalOffset = quot heightTxt 2
      l = zipWith (\i txts ->
                    let firstTxt = head txts
                        lastTxt = last txts
                        rightAlign pos =
                          move verticalOffset Up .
                          move (dy*i) Down .
                          alignTxt (mkRightAlign pos)
                        fromAligned = rightAlign refFrom firstTxt
                        toAligned   = rightAlign refTo lastTxt
                    in (txts, fromAligned, toAligned))
                  [0..] listTxts
  in  mkSequentialTextTranslationsStringAnchored l duration

mkTextAnimCenteredUpDown :: (Coords Pos, Coords Pos)
                         -> (Coords Pos, Coords Pos)
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
    in  if null txtUppers || null txtLowers
          then
            TextAnimation [] (Evolution (Successive []) 0 0 id) (mkEaseClock 0 0 id)
          else
            mkSequentialTextTranslationsCharAnchored
              [(txtUppers, centerUpFromAligned, centerUpToAligned),
               (txtLowers, centerDownFromAligned, centerDownToAligned)]
              duration

alignTxt :: Alignment -> ColorString  -> Coords Pos
alignTxt (Alignment al pos) txt =
  uncurry move (align al $ countChars txt) pos
