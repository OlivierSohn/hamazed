{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Animation
    ( Animation(..)
    , mkAnimation
    , mkAnimationTree
    , earliestDeadline
    , renderAnimations
    -- | animations
    , simpleExplosion
    , quantitativeExplosionThenSimpleExplosion
    , simpleLaser
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
                    , Segment
                    , mkSegment
                    , showSegment
                    , translatedFullCircle
                    , translatedFullCircleFromQuarterArc
                    , bresenham )
import           Render( RenderState
                       , renderPoints )
import           Timing( KeyTime
                       , addAnimationStepDuration
                       , animationSpeed)
import           WorldSize(Location(..))


newtype Iteration = Iteration Int deriving(Generic, Eq, Show, Num)

zeroIteration :: Iteration
zeroIteration = Iteration 0

nextIteration :: Iteration -> Iteration
nextIteration (Iteration i) = Iteration $ i + animationSpeed

previousIteration :: Iteration -> Iteration
previousIteration (Iteration i) = Iteration $ i - animationSpeed

data StepType = Update
              | Same

data Animation = Animation {
    _animationNextTime :: !KeyTime
  , _animationCounter :: !Iteration
  , _animationRender :: !(StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation))
}

mkAnimation :: (StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation))
            -> KeyTime
            -> Animation
mkAnimation render t = Animation t {-do not increment, it will be done while rendering-} zeroIteration render


getStep :: Maybe KeyTime -> Animation -> StepType
getStep mayKey (Animation k' i _)
  | i == zeroIteration = Update -- initialize step
  | otherwise          = maybe Same (\k -> if k == k' then Update else Same) mayKey

-- \ This datastructure is used to keep a state of the animation progress, not globally,
--   but locally on each animation point. It is also recursive, so that we can sequence
--   multiple animations.
data Tree = Tree {
    _treeRoot :: !Coords
    -- ^ where the animation begins
  , _treeStart :: !Iteration
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
combine points uncheckedPreviousPoints iteration getLocation =
  let previousPoints = assert (length points == length uncheckedPreviousPoints) uncheckedPreviousPoints
  in zipWith (combinePoints getLocation iteration) points previousPoints

combinePoints :: (Coords -> Location)
              -> Iteration
              -> Coords
              -> Either Tree Coords
              -> Either Tree Coords
combinePoints getLocation iteration point =
  either Left (\prevPoint -> let trajectory = bresenham (mkSegment prevPoint point) -- prevPoint is collision-free, so we drop it
                                 collision = firstCollision getLocation trajectory
                             in  maybe
                                   (Right point)
                                   (\_ -> Left $ Tree prevPoint (previousIteration iteration) Nothing)
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

chain2AnimationsOnCollision :: (Coords -> Iteration -> [Coords])
                            -- ^ animation 1
                            -> (Coords -> Iteration -> [Coords])
                            -- ^ animation 2
                            -> Iteration
                            -> (Coords -> Location)
                            -- ^ collision function
                            -> Tree
                            -> Tree
chain2AnimationsOnCollision anim1 anim2 iteration getLocation tree  =
  let (Tree a b branches) = applyAnimation anim1 iteration getLocation tree
      newBranches = Just $ case branches of
        Nothing -> error "animateUntilCollision was supposed to create a Just ?"
        Just l ->  map (either (Left . applyAnimation anim2 iteration getLocation) Right) l
  in Tree a b newBranches

applyAnimation :: (Coords -> Iteration -> [Coords])
               -> Iteration
               -> (Coords -> Location)
               -> Tree
               -> Tree
applyAnimation animation globalIteration getLocation (Tree root startIteration branches) =
  let iteration = globalIteration - startIteration
      points = animation root iteration
      newBranches = combine points (fromMaybe (map Right points) branches) iteration getLocation
  in Tree root startIteration $ Just newBranches

simpleExplosionPure :: Coords -> Iteration -> [Coords]
simpleExplosionPure center (Iteration iteration) =
  let radius = fromIntegral iteration :: Float
      resolution = 8
  in translatedFullCircleFromQuarterArc center radius 0 resolution

quantitativeExplosionPure :: Int -> Coords -> Iteration -> [Coords]
quantitativeExplosionPure number center (Iteration iteration) =
  let numRand = 10 :: Int
      rnd = 2 :: Int -- TODO store the random number in the state of the animation
  -- rnd <- getStdRandom $ randomR (0,numRand-1)
      radius = fromIntegral iteration :: Float
      firstAngle = (fromIntegral rnd :: Float) * 2*pi / (fromIntegral numRand :: Float)
  in translatedFullCircle center radius firstAngle number

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
        a' = (case step of
          Update -> stepAnimation
          Same   -> id) a
    render step a' getLocation r) anims

setRender :: Animation
          -> (StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation))
          -> Animation
setRender (Animation t i _) = Animation t i

simpleExplosion :: Tree -> StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation)
simpleExplosion state step a@(Animation _ i _) getLocation s = do
  let newState = case step of
        Update -> applyAnimation simpleExplosionPure i getLocation state
        Same -> state
      points = getAliveCoordinates newState
  renderAnimation points (setRender a $ simpleExplosion newState) s

simpleLaser :: Segment -> Char -> StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation)
simpleLaser seg laserChar _ a@(Animation _ (Iteration i) _) _ state = do
  let points = showSegment seg
      replacementChar = case laserChar of
        '|' -> '.'
        '=' -> '-'
        _ -> error "unsupported case in simpleLaser"
      char = if assert (i > 0) i > animationSpeed then replacementChar else laserChar
  renderPoints char state points
  return $ if assert (i > 0) i > 2 * animationSpeed then Nothing else Just a

quantitativeExplosionThenSimpleExplosion :: Int -> Tree -> StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation)
quantitativeExplosionThenSimpleExplosion number state step a@(Animation _ i _) getLocation = do
  let newState = case step of
        Update -> chain2AnimationsOnCollision (quantitativeExplosionPure number) simpleExplosionPure i getLocation state
        Same   -> state
      points = getAliveCoordinates newState
  renderAnimation points (setRender a $ quantitativeExplosionThenSimpleExplosion number newState)


renderAnimation :: [Coords] -> Animation -> RenderState -> IO (Maybe Animation)
renderAnimation points a state = do
  renderPoints '.' state points
  return $ if null points then Nothing else Just a
