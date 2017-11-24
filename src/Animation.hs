{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}

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
import           Data.Either( partitionEithers )
import           Data.Maybe( catMaybes
                           , fromMaybe )

import           GHC.Generics( Generic )
import           Control.Exception( assert )

import           Collision( firstCollision )
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
import           Render( RenderState
                       , renderPoints )
import           Resample( resample )
import           Timing( KeyTime
                       , addAnimationStepDuration )
import           WorldSize(Location(..))


newtype Iteration = Iteration (Speed, Frame) deriving(Generic, Eq, Show)
newtype Frame = Frame Int deriving(Generic, Eq, Show, Num)
newtype Speed = Speed Int deriving(Generic, Eq, Show, Num)

{-# INLINE zeroIteration #-}
zeroIteration :: Speed -> Iteration
zeroIteration s = Iteration (s,zeroFrame)

{-# INLINE zeroFrame #-}
zeroFrame :: Frame
zeroFrame = Frame 0

{-# INLINE nextIteration #-}
nextIteration :: Iteration -> Iteration
nextIteration (Iteration(s@(Speed speed), Frame i)) = Iteration (s, Frame (i + speed))

{-# INLINE previousIteration #-}
previousIteration :: Iteration -> Iteration
previousIteration (Iteration(s@(Speed speed), Frame i)) = Iteration (s, Frame (i - speed))

data StepType = Update
              | Same

data Animation = Animation {
    _animationNextTime :: !KeyTime
  , _animationIteration :: !Iteration
  , _animationRender :: !(StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation))
}

mkAnimation :: (StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation))
            -> KeyTime
            -> Speed
            -> Animation
mkAnimation render t speed = Animation t {-do not increment, it will be done while rendering-} (zeroIteration speed) render


getStep :: Maybe KeyTime -> Animation -> StepType
getStep mayKey (Animation k' (Iteration(_,frame)) _)
  | frame == zeroFrame = Update -- initialize step
  | otherwise          = maybe Same (\k -> if k == k' then Update else Same) mayKey

applyStep :: StepType -> Animation -> Animation
applyStep = \case
               Update -> stepAnimation
               Same   -> id

-- \ This datastructure is used to keep a state of the animation progress, not globally,
--   but locally on each animation point. It is also recursive, so that we can sequence
--   multiple animations.
data Tree = Tree {
    _treeRoot :: !Coords
    -- ^ where the animation begins
  , _treeStart :: !Frame
    -- ^ when the animation begins (relatively to the parent animation if any)
  , _treeBranches :: !(Maybe [Either Tree Coords])
    -- ^ There is one element in the list per animation point.
    -- 'Right Coords' elements are still alive (typically they didn't collide yet with the world).
    -- 'Left Tree' elements are dead for this animation and maybe gave birth to another animation.
}

mkAnimationTree :: Coords -> Tree
mkAnimationTree c = Tree c 0 Nothing

getAliveCoordinates :: Tree -> [Coords]
getAliveCoordinates (Tree _ _ Nothing) = []
getAliveCoordinates (Tree _ _ (Just [])) = []
getAliveCoordinates (Tree _ _ (Just branches)) =
  let (children, aliveCoordinates) = partitionEithers branches
  in concatMap getAliveCoordinates children ++ aliveCoordinates


combine :: [Coords]
        -> [Either Tree Coords]
        -> Iteration
        -> (Coords -> Location)
        -> [Either Tree Coords]
combine points uncheckedPreviousState iteration getLocation =
  let previousState = assert (length points == length uncheckedPreviousState) uncheckedPreviousState
  in zipWith (combinePoints getLocation iteration) points previousState

combinePoints :: (Coords -> Location)
              -> Iteration
              -> Coords
              -> Either Tree Coords
              -> Either Tree Coords
combinePoints getLocation iteration point =
  either Left (\prevPoint -> let trajectory = bresenham (mkSegment (assert (getLocation prevPoint == InsideWorld) prevPoint) point)
                                 collision =  firstCollision getLocation trajectory
                             in  maybe
                                   (Right $ assert (getLocation point == InsideWorld) point)
                                   (\(_, preCollisionCoords) ->
                                        -- TODO use currentFrame instead of previous and verify combining animations look good:
                                        -- using the previous was an historical choice when there was no notion of trajectory
                                        -- but now, since here we move to the precoliision, it makes sense to not skip a frame
                                        -- anymore
                                        let (Iteration(_,frame)) = previousIteration iteration
                                        in Left $ Tree preCollisionCoords frame Nothing)
                                   collision)

-- TODO generic chaining of animations
{--
chainAnimationsOnCollision :: [Coords -> Iteration -> [Coords]]
                           -- ^ each animation function should return a constant number of Coords across iterations
                           -> Iteration
                           -> (Coords -> Location)
                           -- ^ collision function
                           -> Tree
                           -> Tree
chainAnimationsOnCollision animations iteration getLocation tree = undefined
--}

chain2AnimationsOnCollision :: (Coords -> Frame -> [Coords])
                            -- ^ animation 1
                            -> (Coords -> Frame -> [Coords])
                            -- ^ animation 2
                            -> Iteration
                            -> (Coords -> Location)
                            -- ^ collision function
                            -> Tree
                            -> Tree
chain2AnimationsOnCollision anim1 anim2 iteration getLocation tree  =
  let (Tree a b branches) = applyAnimation anim1 iteration getLocation tree
      newBranches = Just $ case branches of
        Nothing -> error "applyAnimation was supposed to create a Just ?"
        Just l ->  map (either (Left . applyAnimation anim2 iteration getLocation) Right) l
  in Tree a b newBranches

applyAnimation :: (Coords -> Frame -> [Coords])
               -> Iteration
               -> (Coords -> Location)
               -> Tree
               -> Tree
applyAnimation animation iteration@(Iteration (_,globalFrame)) getLocation (Tree root startFrame branches) =
  let frame = globalFrame - startFrame
      points = animation root frame
      previousState = fromMaybe (replicate (length points) $ Right $ assert (getLocation root == InsideWorld) root) branches
      -- if previousState contains only Left(s), the animation does not need to be computed.
      -- I wonder if lazyness takes care of that or not?
      newBranches = combine points previousState iteration getLocation
  in Tree root startFrame $ Just newBranches

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
  let centerBar = move (assert (i > 0) i) dir first
      orthoDir = rotateCcw 1 dir
      startBar = move i orthoDir centerBar
      endBar = move (-i) orthoDir centerBar
  in  connect2 startBar endBar

polygon :: Int -> Coords -> Frame -> [Coords]
polygon nSides center (Frame i) =
  let startAngle = if odd nSides then pi else pi/4.0
      extremities = polyExtremities nSides center i startAngle
  in connect extremities

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

renderAnimations :: Maybe KeyTime -> (Coords -> Location) -> RenderState -> [Animation] -> IO [Animation]
renderAnimations k getLocation r anims =
  catMaybes <$> mapM (\a@(Animation _ _ render) -> do
    let step = getStep k a
        a' = applyStep step a
    render step a' getLocation r) anims

setRender :: Animation
          -> (StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation))
          -> Animation
setRender (Animation t i _) = Animation t i

simpleLaser :: Segment -> Char -> StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation)
simpleLaser seg laserChar _ a@(Animation _ (Iteration (Speed speed, Frame i)) _) _ state = do
  let points = showSegment seg
      replacementChar = case laserChar of
        '|' -> '.'
        '=' -> '-'
        _ -> error "unsupported case in simpleLaser"
      iterationUseReplacement = 2 * speed
      iterationStop = 4 * speed
      char = if assert (i > 0) i > iterationUseReplacement then replacementChar else laserChar
  renderPoints char state points
  return $ if assert (i > 0) i > iterationStop then Nothing else Just a

quantitativeExplosionThenSimpleExplosion :: Int -> Tree -> StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation)
quantitativeExplosionThenSimpleExplosion number = animate fPure f
  where
    fPure = chain2AnimationsOnCollision (quantitativeExplosionPure number) (simpleExplosionPure 8)
    f = quantitativeExplosionThenSimpleExplosion number

simpleExplosion :: Int -> Tree -> StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation)
simpleExplosion resolution = animate fPure f
  where
    fPure = applyAnimation (simpleExplosionPure resolution)
    f = simpleExplosion resolution

gravityExplosionThenSimpleExplosion :: Vec2 -> Tree -> StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation)
gravityExplosionThenSimpleExplosion initialSpeed = animate fPure f
  where
    fPure = chain2AnimationsOnCollision (gravityExplosionPure initialSpeed) (simpleExplosionPure 8)
    f = gravityExplosionThenSimpleExplosion initialSpeed


gravityExplosion :: Vec2 -> Tree -> StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation)
gravityExplosion initialSpeed = animate fPure f
  where
    fPure = applyAnimation (gravityExplosionPure initialSpeed)
    f = gravityExplosion initialSpeed

animatedNumber :: Int -> Tree -> StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation)
animatedNumber n =
  animate' (mkAnimator animateNumberPure animatedNumber n)

data Animator a = Animator {
    _animatorPure :: !(Iteration -> (Coords -> Location) -> Tree -> Tree)
  , _animatorIO   :: !(Tree -> StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation))
}

mkAnimator :: (t -> Coords -> Frame -> [Coords])
           -> (t
               -> Tree
               -> StepType
               -> Animation
               -> (Coords -> Location)
               -> RenderState
               -> IO (Maybe Animation))
           -> t
           -> Animator a
mkAnimator pure_ io_ params = Animator (applyAnimation (pure_ params)) (io_ params)

-- if this function is not inlined, in optimized mode, the program loops forever when trigerring the animation. TODO test with latest GHC
{-# INLINE animate' #-}
animate' :: Animator a -> Tree -> StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation)
animate' (Animator pure_ io_) = animate pure_ io_

animate :: (Iteration -> (Coords -> Location) -> Tree -> Tree)
        -- ^ the pure animation function
        -> (Tree -> StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation))
        -- ^ the IO animation function
        ->  Tree -> StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation)
animate pureAnim ioAnim state step a@(Animation _ i _) getLocation = do
  let newState = case step of
        Update -> pureAnim i getLocation state
        Same -> state
  renderAnimation (getAliveCoordinates newState) (setRender a $ ioAnim newState)

renderAnimation :: [Coords] -> Animation -> RenderState -> IO (Maybe Animation)
renderAnimation points a state = do
  renderPoints '.' state points
  return $ if null points then Nothing else Just a
