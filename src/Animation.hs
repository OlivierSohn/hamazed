{-# LANGUAGE NoImplicitPrelude #-}

module Animation
    ( mkAnimation
    , mkAnimationTree
    -- | animations
    , simpleExplosion
    , gravityExplosion
    , gravityExplosionThenSimpleExplosion
    , quantitativeExplosionThenSimpleExplosion
    , simpleLaser
    , animatedNumber
    -- | preapplied animations
    , explosion
    , explosion1
    ) where


import           Imajuscule.Prelude

import           Animation.Design.Animator
import           Animation.Design.Apply
import           Animation.Design.Chain
import           Animation.Design.Geo
import           Animation.Design.RenderUpdate
import           Animation.Types
import           Color
import           Geo
import           Render( RenderState, renderColoredPoints )
import           WorldSize( Location )

simpleLaser :: Segment -> Char -> StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation)
simpleLaser seg laserChar _ a@(Animation _ (Iteration (Speed speed, f@(Frame i))) _ _) _ state = do
  let points = if assert (i > 0) i > iterationStop
                 then
                   []
                 else
                   showSegment seg
      replacementChar = case laserChar of
        '|' -> '.'
        '=' -> '-'
        _ -> error "unsupported case in simpleLaser"
      iterationUseReplacement = 2 * speed
      iterationStop = 4 * speed
      char = if i > iterationUseReplacement then replacementChar else laserChar
      nextAnimation = if null points
                        then
                          Nothing
                        else
                          Just a
  renderColoredPoints char points (colorFromFrame f) state
  return nextAnimation

quantitativeExplosionThenSimpleExplosion :: Int -> Tree -> StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation)
quantitativeExplosionThenSimpleExplosion number = renderAndUpdate fPure f colorFromFrame
  where
    fPure = chainOnCollision (quantitativeExplosionPure number) (simpleExplosionPure 8)
    f = quantitativeExplosionThenSimpleExplosion number

simpleExplosion :: Int -> Tree -> StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation)
simpleExplosion resolution = renderAndUpdate fPure f colorFromFrame
  where
    fPure = applyAnimation (simpleExplosionPure resolution)
    f = simpleExplosion resolution

gravityExplosionThenSimpleExplosion :: Vec2 -> Tree -> StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation)
gravityExplosionThenSimpleExplosion initialSpeed = renderAndUpdate fPure f colorFromFrame
  where
    fPure = chainOnCollision (gravityExplosionPure initialSpeed) (simpleExplosionPure 8)
    f = gravityExplosionThenSimpleExplosion initialSpeed

gravityExplosion :: Vec2 -> Tree -> StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation)
gravityExplosion initialSpeed = renderAndUpdate fPure f colorFromFrame
  where
    fPure = applyAnimation (gravityExplosionPure initialSpeed)
    f = gravityExplosion initialSpeed

animatedNumber :: Int -> Tree -> StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation)
animatedNumber n =
  renderAndUpdate' (mkAnimator animateNumberPure animatedNumber n)


explosion :: Vec2
          -> Coords
          -> [StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation)]
explosion sp pos =
  let variations = [ Vec2 0.3     (-0.4)
                   , Vec2 (-0.55) (-0.29)
                   , Vec2 (-0.1)  0.9
                   , Vec2 1.2     0.2]
      speeds = map (sumVec2d sp) variations
  in map (`explosion1` pos) speeds

explosion1 :: Vec2
           -> Coords
           -> (StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation))
explosion1 speed pos =
  gravityExplosionThenSimpleExplosion speed (mkAnimationTree pos (ReboundAnd $ ReboundAnd Stop))
