{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Animation
    ( Animation(..)
    , Speed(..)
    , mkAnimation
    , mkAnimationTree
    , earliestDeadline
    , renderAnimations
    -- | animations
    , simpleExplosion
    , gravityExplosion
    , gravityExplosionThenSimpleExplosion
    , quantitativeExplosionThenSimpleExplosion
    , simpleLaser
    , animatedNumber
    ) where


import           Imajuscule.Prelude

import           Data.List( length )
import           Data.Maybe( catMaybes )

import           Animation.Animate
import           Animation.Animator
import           Animation.Types
import           Animation.Design.Apply
import           Animation.Design.Chain
import           Color
import           Geo( Coords
                    , bresenham
                    , bresenhamLength
                    , Segment
                    , Direction(..)
                    , mkSegment
                    , move
                    , polyExtremities
                    , rotateCcw
                    , showSegment
                    , translatedFullCircle
                    , translatedFullCircleFromQuarterArc
                    , parabola
                    , Vec2(..)
                    , pos2vec
                    , vec2coords )
import           Render( RenderState, renderColored )
import           Resample( resample )
import           Timing( KeyTime
                       , addAnimationStepDuration )
import           WorldSize( Location(..) )



computeStep :: Maybe KeyTime -> Animation -> StepType
computeStep mayKey (Animation k' (Iteration(_,frame)) _)
  | frame == zeroFrame = Update -- initialize step
  | otherwise          = maybe Same (\k -> if k == k' then Update else Same) mayKey

update :: StepType -> Animation -> Animation
update = \case
            Update -> stepAnimation
            Same   -> id

gravityExplosionPure :: Vec2 -> Coords -> Frame -> [Coords]
gravityExplosionPure initialSpeed origin (Frame iteration) =
  let o = pos2vec origin
  in  [vec2coords $ parabola o initialSpeed iteration]

simpleExplosionPure :: Int -> Coords -> Frame -> [Coords]
simpleExplosionPure resolution center (Frame iteration) =
  let radius = fromIntegral iteration :: Float
      c = pos2vec center
  in map vec2coords $ translatedFullCircleFromQuarterArc c radius 0 resolution

quantitativeExplosionPure :: Int -> Coords -> Frame -> [Coords]
quantitativeExplosionPure number center (Frame iteration) =
  let numRand = 10 :: Int
      rnd = 2 :: Int -- TODO store the random number in the state of the animation
  -- rnd <- getStdRandom $ randomR (0,numRand-1)
      radius = fromIntegral iteration :: Float
      firstAngle = (fromIntegral rnd :: Float) * 2*pi / (fromIntegral numRand :: Float)
      c = pos2vec center
  in map vec2coords $ translatedFullCircle c radius firstAngle number

animateNumberPure :: Int -> Coords -> Frame -> [Coords]
animateNumberPure 1 = simpleExplosionPure 8
animateNumberPure 2 = rotatingBar Up
animateNumberPure n = polygon n

-- TODO make it rotate, like the name says :)
rotatingBar :: Direction -> Coords -> Frame -> [Coords]
rotatingBar dir first (Frame i) =
  let radius = animateRadius (assert (i > 0) i) 2
      centerBar = move i dir first
      orthoDir = rotateCcw 1 dir
      startBar = move radius orthoDir centerBar
      endBar = move (-radius) orthoDir centerBar
  in  connect2 startBar endBar

polygon :: Int -> Coords -> Frame -> [Coords]
polygon  nSides center (Frame i) =
  let startAngle = if odd nSides then pi else pi/4.0
      radius = animateRadius (1 + quot i 2) nSides
      extremities = polyExtremities nSides center radius startAngle
  in if radius <= 0
       then
         []
       else
         connect extremities

animateRadius :: Int -> Int -> Int
animateRadius i nSides =
  let limit
          | nSides <= 4 = 5
          | nSides <= 6 = 7
          | otherwise   = 10
  in if i < limit
       then
         i
       else
         max 0 (2 * limit - i)

connect :: [Coords] -> [Coords]
connect []  = []
connect l@[_] = l
connect (a:rest@(b:_)) = connect2 a b ++ connect rest

connect2 :: Coords -> Coords -> [Coords]
connect2 start end =
  let numpoints = 80 -- more than 2 * (max height width of world) to avoid spaces
  in sampledBresenham numpoints start end

sampledBresenham :: Int -> Coords -> Coords -> [Coords]
sampledBresenham nSamples start end =
  let l = bresenhamLength start end
      seg = mkSegment start end
      bres = bresenham seg
  in resample bres (assert (l == length bres) l) nSamples

stepAnimation :: Animation -> Animation
stepAnimation (Animation t i f) = Animation (addAnimationStepDuration t) (nextIteration i) f

earliestDeadline :: [Animation] -> Maybe KeyTime
earliestDeadline animations =
  if null animations
    then
      Nothing
    else
      Just $ minimum $ map (\(Animation deadline _ _) -> deadline) animations


--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

-- renders and updates animations
renderAnimations :: Maybe KeyTime -> (Coords -> Location) -> RenderState -> [Animation] -> IO [Animation]
renderAnimations k getLocation r anims =
  catMaybes <$> mapM (\a@(Animation _ _ render) -> do
    let step = computeStep k a
        a' = update step a
    render step a' getLocation r) anims

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
