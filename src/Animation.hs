{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}

module Animation
    ( Animation(..)
    , mkAnimation
    , stepEarliest
    , earliestDeadline
    , simpleExplosion
    , simpleExplosionUntilCollisions
    , quantitativeExplosionThenSimpleExplosion
    , renderAnimations
    ) where

import           Data.Either( rights )
import           Data.List( partition )
import           Data.Maybe( catMaybes )
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

-- | A structure used to sequence animations.
data AnimationOrigin = AnimationOrigin {
    -- The reference point, wrt World frame. Typically it will be the last point
    -- of the previous animation in the sequence.
    _animationOriginCoords :: !Coords
    -- when Just, it is the iteration (of the previous animation in the sequence)
    -- at which the animation started. If Nothing, the animation has not yet started.
  , _animationOriginFirstIteration :: !(Maybe Iteration)
}

-- | This function tests if the second animation has already begun. If it has begun, it continues it.
-- Else If first animation is outside the world, it starts second animation.
-- Else, it continues first animation.
firstAnimationWhileInsideWorldThenSecondAnimation :: (Coords -> Iteration -> [Coords])
                                                  -- ^ the second animation function (parameters are center and iteration)
                                                  -> (Coords -> Location)
                                                  -> Iteration
                                                  -> (Coords, AnimationOrigin)
                                                  -- ^ (first animation point, current second animation origin)
                                                  -> ([Coords], AnimationOrigin)
                                                  -- ^ (resulting animation points : either [first animation point] or results of the second animation
                                                  --    , possibly modified origin of second animation (if second animation has not started yet or has just started)))
firstAnimationWhileInsideWorldThenSecondAnimation anim2 getLocation iteration (anim1Point, col@(AnimationOrigin anim2Origin mayAnim2FirstIteration)) =
 case mayAnim2FirstIteration of
  (Just anim2FirstIteration) ->
  -- the second animation has started already
    let anim2Iteration = iteration - anim2FirstIteration
    in (anim2 anim2Origin anim2Iteration, col)
  Nothing -> case getLocation anim1Point of
    OutsideWorld ->
    -- This is the first iteration where we are outside. anim1Point is the previous point
    -- which was set in the previous iteration, in the 'InsideWorld' match of this pattern match
    --
    -- Also, at the previous iteration the resulting point was equivalent to the call hereunder
    -- with a radius 0 so we use a radius 1 here and make sure to record the previous iteration
    -- as the start of the 2nd animation, so that at the next iteration the radius will increase (= 2).
      (anim2 anim2Origin 1, AnimationOrigin anim2Origin $ Just $ previousIteration iteration)
    InsideWorld ->
    -- the first animation has not ended yet.
      ([anim1Point], AnimationOrigin anim1Point Nothing)


-- | In this function, 'Coords' in the types of functions passed as arguments are animation origins.
anim1WhileInsideWorldThenAnim2 :: (Coords -> Iteration -> [Coords])
                               -- ^ First animation. Regardless of iteration number, it should
                               -- returns the same number of points, and that 'same index' points
                               -- correlate across iterations.
                               -> (Coords -> Iteration -> [Coords])
                               -- ^ Second animation.
                               -> (Coords -> Location)
                               -> [AnimationOrigin]
                               -> Coords
                               -> Iteration
                               -> ([Coords], [AnimationOrigin])
anim1WhileInsideWorldThenAnim2 anim1 anim2 getLocation prevAnim2Origins center iteration =
  let anim1Points = anim1 center iteration
      anim2Origins =
        if null prevAnim2Origins
        then map (`AnimationOrigin` Nothing) anim1Points
        else prevAnim2Origins
      (pointsLists, newAnim2Origins) = unzip $
        map (firstAnimationWhileInsideWorldThenSecondAnimation anim2 getLocation iteration) $
         zip anim1Points anim2Origins
  in (concat pointsLists, newAnim2Origins)

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

renderCharIfInFrame :: Char -> Coords -> (Coords -> Location) -> RenderState -> IO Location
renderCharIfInFrame char pos getLocation (RenderState upperLeftCoords) = do
  let loc = getLocation pos
  case loc of
    OutsideWorld -> return ()
    InsideWorld -> renderChar_ char $ RenderState $ sumCoords pos upperLeftCoords
  return loc

setRender :: Animation
          -> (Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation))
          -> Animation
setRender (Animation t i _) = Animation t i

simpleExplosion :: Coords -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation)
simpleExplosion center a@(Animation _ i _) = do
  let points = simpleExplosionPure center i
  renderAnimation points $ setRender a $ simpleExplosion center


simpleExplosionUntilCollisions :: [Either (Coords, Iteration) Coords] -> Coords -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation)
simpleExplosionUntilCollisions previousPoints center a@(Animation _ i _) getLocation s = do
  let points = animateUntilCollision (simpleExplosionPure center) previousPoints i getLocation
  renderAnimation (rights points) (setRender a $ simpleExplosionUntilCollisions points center) getLocation s

{--
quantitativeExplosion :: Int -> Coords -> Animation -> Space -> RenderState -> IO (Maybe Animation)
quantitativeExplosion number center a@(Animation _ i _) sz = do
  let originalPoints = quantitativeExplosionPure number center i
      points = mapMaybe (reboundMaxRecurse sz 4) originalPoints
  renderAnimation points (setRender a $ quantitativeExplosion number center) sz
--}

quantitativeExplosionThenSimpleExplosion :: [AnimationOrigin] -> Int -> Coords -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation)
quantitativeExplosionThenSimpleExplosion anim2Origins number center a@(Animation _ i _) sz = do
  let (points, newAnim2Origins) = anim1WhileInsideWorldThenAnim2 (quantitativeExplosionPure number) simpleExplosionPure sz anim2Origins center i
  renderAnimation points (setRender a $ quantitativeExplosionThenSimpleExplosion newAnim2Origins number center) sz


renderAnimation :: [Coords] -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation)
renderAnimation points a getLocation state = do
  loc <- renderPoints points '.' getLocation state
  return $ if loc == OutsideWorld then Nothing else Just a

renderPoints :: [Coords] -> Char -> (Coords -> Location) -> RenderState -> IO Location
renderPoints points char getLocation state = do
  locations <- mapM (\c -> renderCharIfInFrame char c getLocation state) points
  return $ if null locations || all (== OutsideWorld) locations then OutsideWorld else InsideWorld


animateUntilCollision :: (Iteration -> [Coords])
                      -- ^ animation function which always returns the same number of Coords
                      -> [Either (Coords, Iteration) Coords]
                      -- ^ (empty for first iteration)
                      --   Left  : last non-colliding coord and iteration,
                      --   Right : non colliding coord (we don't know if the next coord will collide or not)
                      -> Iteration
                      -> (Coords -> Location)
                      -- ^ collision function
                      -> [Either (Coords, Iteration) Coords]
animateUntilCollision animation mayPreviousState iteration =
  let iterationResult = animation iteration
      previousState = if null mayPreviousState
        then
          map Right iterationResult
        else
          mayPreviousState
  in combine iterationResult previousState iteration

combine :: [Coords]
        -> [Either (Coords, Iteration) Coords]
        -> Iteration
        -> (Coords -> Location)
        -> [Either (Coords, Iteration) Coords]
combine points uncheckedPreviousPoints iteration getLocation =
  let previousPoints = assert (length points == length uncheckedPreviousPoints) uncheckedPreviousPoints
  in zipWith (combinePoints getLocation iteration) points previousPoints

combinePoints :: (Coords -> Location)
              -> Iteration
              -> Coords
              -> Either (Coords, Iteration) Coords
              -> Either (Coords, Iteration) Coords
combinePoints getLocation iteration point =
  either Left (\prevPoint ->
                  case getLocation point of
                    OutsideWorld -> Left (prevPoint, previousIteration iteration)
                    InsideWorld -> Right point)

chainAnimationsOnCollision :: (Coords -> Location)
                           -> [Iteration -> [Coords]]
                          -- ^ each animation function should return a constant number of Coords across iterations
                           -> (Iteration -> [Coords])
                           -- ^ the returned animation function (can return different numbers of coords across iterations)
chainAnimationsOnCollision getLocation animations = undefined
