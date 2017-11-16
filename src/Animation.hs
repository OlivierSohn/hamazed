{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Animation
    ( Animation(..)
    , mkAnimation
    , mkAnimationRoot
    , Tree -- constructors are hidden
    , stepEarliest
    , earliestDeadline
    , simpleExplosionUntilCollisions
    , quantitativeExplosionThenSimpleExplosionUntilCollision
    , renderAnimations
    ) where

import           Data.List( partition )
import           Data.Either( partitionEithers )
import           Data.Maybe( catMaybes
                           , fromMaybe )
import           Data.Time( addUTCTime
                          , NominalDiffTime
                          , UTCTime )

import           GHC.Generics( Generic )
import           Control.Exception( assert )
--import           System.Random( getStdRandom
--                              , randomR )


import           Console( RenderState(..)
                        , renderChar_ )
import           Geo( Coords(..)
                    , sumCoords
                    , translatedFullCircle
                    , translatedFullCircleFromQuarterArc )
import           WorldSize(Location(..))


newtype Iteration = Iteration Int deriving(Generic, Eq, Show, Num)

zeroIteration :: Iteration
zeroIteration = Iteration 0

nextIteration :: Iteration -> Iteration
nextIteration (Iteration i) = Iteration $ succ i

previousIteration :: Iteration -> Iteration
previousIteration (Iteration i) = Iteration $ pred i

data Animation = Animation {
    _animationNextTime :: !UTCTime
  , _animationCounter  :: !Iteration
  , _animationRender :: !(Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation))
}

data AnimationProgress = AnimationInProgress
                       | AnimationDone
                       deriving(Eq, Show)

mkAnimation :: (Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation))
            -> UTCTime
            -> Animation
mkAnimation render currentTime = Animation (addUTCTime animationPeriod currentTime) zeroIteration render


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


animationPeriod :: Data.Time.NominalDiffTime
animationPeriod = 0.02

-- step the animations whose deadline are the earliest
stepEarliest :: [Animation] -> [Animation]
stepEarliest l = let (earliest, latest) = partitionEarliest l
                 in latest ++ map stepAnimation earliest

partitionEarliest :: [Animation] -> ([Animation], [Animation])
partitionEarliest l =
    maybe ([],[]) partitionOnDeadline (earliestDeadline l)
  where
    partitionOnDeadline deadline = partition (\(Animation deadline' _ _) -> deadline' == deadline) l

stepAnimation :: Animation -> Animation
stepAnimation (Animation t i f) = Animation (addUTCTime animationPeriod t) (nextIteration i) f

earliestDeadline :: [Animation] -> Maybe UTCTime
earliestDeadline []         = Nothing
earliestDeadline animations = Just $ minimum $ map (\(Animation deadline _ _) -> deadline) animations


--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------


renderAnimations :: (Coords -> Location) -> RenderState -> [Animation] -> IO [Animation]
renderAnimations getLocation r anims = do
  newAnims <- mapM (\a@(Animation _ _ render) -> (render a getLocation r)) anims
  return $ catMaybes newAnims

renderChar :: Char -> Coords -> RenderState -> IO ()
renderChar char pos (RenderState upperLeftCoords) =
  renderChar_ char $ RenderState $ sumCoords pos upperLeftCoords

setRender :: Animation
          -> (Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation))
          -> Animation
setRender (Animation t i _) = Animation t i

simpleExplosionUntilCollisions :: Tree -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation)
simpleExplosionUntilCollisions state@(Tree center _ _) a@(Animation _ i _) getLocation s = do
  let newState = animateUntilCollision (simpleExplosionPure center) state i getLocation
      points = getLeaves newState
  renderAnimation points (setRender a $ simpleExplosionUntilCollisions newState) getLocation s

quantitativeExplosionThenSimpleExplosionUntilCollision :: Int -> Tree -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation)
quantitativeExplosionThenSimpleExplosionUntilCollision number state@(Tree center _ _) a@(Animation _ i _) getLocation = do
  let newState = chain2AnimationsOnCollision (quantitativeExplosionPure number center) simpleExplosionPure i getLocation state
      points = getLeaves newState
  renderAnimation points (setRender a $ quantitativeExplosionThenSimpleExplosionUntilCollision number newState) getLocation


renderAnimation :: [Coords] -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation)
renderAnimation points a getLocation state = do
  renderPoints points '.' state
  return $ if null points then Nothing else Just a

renderPoints :: [Coords] -> Char -> RenderState -> IO ()
renderPoints points char state =
  mapM_ (\c -> renderChar char c state) points


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
  either Left (\prevPoint ->
                  case getLocation point of
                    OutsideWorld -> Left $ Tree prevPoint (previousIteration iteration) Nothing
                    InsideWorld -> Right point)

chainAnimationsOnCollision :: (Coords -> Location)
                           -> [Iteration -> [Coords]]
                           -- ^ each animation function should return a constant number of Coords across iterations
                           -> (Iteration -> [Coords])
                           -- ^ the returned animation function (can return different numbers of coords across iterations)
chainAnimationsOnCollision getLocation animations = undefined

-- \ This datastructure allows to sequence animations easily.
--
-- An animation is typically defined by an origin coordinate, and gives birth to
-- a set of moving points (like an explosion).
-- Once a moving point of the animation collides with the world, the animation
-- typically stops, and maybe another animation starts at that point.
data Tree = Tree {
    _treeRoot :: !Coords
    -- ^ where the animation starts
  , _treeStart :: !Iteration
    -- ^ when the animation starts (relatively to parent if any)
  , _treeBranches :: !(Maybe [Either Tree Coords])
    -- ^ the current animation results : an element is 'Coords' if the animation is still alive
    -- for this element, else 'Tree'. Typically an animation is alive for an element until there is
    -- a collision with the world. This collision my give birth to another animation tree.
}

getLeaves :: Tree -> [Coords]
getLeaves (Tree _ _ Nothing) = []
getLeaves (Tree _ _ (Just [])) = []
getLeaves (Tree _ _ (Just branches)) =
  let (trees, coordinates) = partitionEithers branches
      recurse = concatMap getLeaves trees
  in coordinates ++ recurse

mkAnimationRoot :: Coords -> Tree
mkAnimationRoot c = Tree c 0 Nothing

chain2AnimationsOnCollision :: (Iteration -> [Coords])
                            -- ^ animation 1
                            -> (Coords -> Iteration -> [Coords])
                            -- ^ animation 2 (is passed a reference point)
                            -> Iteration
                            -> (Coords -> Location)
                            -- ^ collision function
                            -> Tree
                            -> Tree
chain2AnimationsOnCollision anim1 anim2 iteration getLocation tree  =
  let (Tree a b branches) = animateUntilCollision anim1 tree iteration getLocation
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

animateUntilCollision :: (Iteration -> [Coords])
                      -- ^ animation function which always returns the same number of Coords
                      -> Tree
                      -> Iteration
                      -> (Coords -> Location)
                      -- ^ collision function
                      -> Tree
animateUntilCollision animation (Tree a b mayPreviousState) iteration getLocation =
  let iterationResult = animation iteration
      previousState = fromMaybe (map Right iterationResult) mayPreviousState
  in Tree a b $ Just (combine iterationResult previousState iteration getLocation)
