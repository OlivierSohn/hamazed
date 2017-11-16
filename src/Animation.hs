{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Animation
    ( Animation(..)
    , mkAnimation
    , mkAnimationTree
    , stepEarliest
    , earliestDeadline
    , renderAnimations
    , animationPeriod
    -- | animations
    , simpleExplosion
    , quantitativeExplosionThenSimpleExplosion
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

mkAnimation :: (Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation))
            -> UTCTime
            -> Animation
mkAnimation render currentTime = Animation (addUTCTime animationPeriod currentTime) zeroIteration render


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
  either Left (\prevPoint ->
                  case getLocation point of
                    OutsideWorld -> Left $ Tree prevPoint (previousIteration iteration) Nothing
                    InsideWorld -> Right point)

-- TODO
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


animationPeriod :: NominalDiffTime
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
earliestDeadline animations =
  if null animations
    then
      Nothing
    else
      Just $ minimum $ map (\(Animation deadline _ _) -> deadline) animations


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

simpleExplosion :: Tree -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation)
simpleExplosion state a@(Animation _ i _) getLocation s = do
  let newState = applyAnimation simpleExplosionPure i getLocation state
      points = getAliveCoordinates newState
  renderAnimation points (setRender a $ simpleExplosion newState) s

quantitativeExplosionThenSimpleExplosion :: Int -> Tree -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation)
quantitativeExplosionThenSimpleExplosion number state a@(Animation _ i _) getLocation = do
  let newState = chain2AnimationsOnCollision (quantitativeExplosionPure number) simpleExplosionPure i getLocation state
      points = getAliveCoordinates newState
  renderAnimation points (setRender a $ quantitativeExplosionThenSimpleExplosion number newState)


renderAnimation :: [Coords] -> Animation -> RenderState -> IO (Maybe Animation)
renderAnimation points a state = do
  renderPoints points '.' state
  return $ if null points then Nothing else Just a

renderPoints :: [Coords] -> Char -> RenderState -> IO ()
renderPoints points char state =
  mapM_ (\c -> renderChar char c state) points
