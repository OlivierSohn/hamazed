{-# LANGUAGE NoImplicitPrelude #-}

module Animation
    ( Animation
    , Speed(..)
    , mkAnimation
    , mkAnimationTree
    -- | animations
    , simpleExplosion
    , gravityExplosion
    , gravityExplosionThenSimpleExplosion
    , quantitativeExplosionThenSimpleExplosion
    , simpleLaser
    , animatedNumber
    ) where


import           Imajuscule.Prelude

import           Animation.Design.Animator
import           Animation.Design.Apply
import           Animation.Design.Chain
import           Animation.Design.Geo
import           Animation.Design.RenderUpdate
import           Animation.Types
import           Color
import           Geo( Coords, Segment, showSegment, Vec2 )
import           Render( RenderState, renderColored )
import           WorldSize( Location )

simpleLaser :: Segment -> Char -> StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation)
simpleLaser seg laserChar _ a@(Animation _ (Iteration (Speed speed, f@(Frame i))) _) _ state = do
  let points = showSegment seg
      replacementChar = case laserChar of
        '|' -> '.'
        '=' -> '-'
        _ -> error "unsupported case in simpleLaser"
      iterationUseReplacement = 2 * speed
      iterationStop = 4 * speed
      char = if assert (i > 0) i > iterationUseReplacement then replacementChar else laserChar
  renderColored char points (colorFromFrame f) state
  return $ if assert (i > 0) i > iterationStop then Nothing else Just a

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
