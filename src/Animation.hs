
module Animation
    ( Animation(..)
    , mkAnimation
    , stepEarliest
    , earliestDeadline
    , simpleExplosion
    , quantitativeExplosion
    , quantitativeExplosionThenSimpleExplosion
    , renderAnimations
    , WorldSize(..)
    ) where


import           Data.List( partition )
import           Data.Maybe( catMaybes
                           , mapMaybe )
import           Data.Time( addUTCTime
                          , NominalDiffTime
                          , UTCTime )
--import           System.Random( getStdRandom
--                              , randomR )


import           Console( RenderState(..)
                        , renderChar_ )
import           Geo( Coords(..)
                    , sumCoords
                    , translatedFullCircle
                    , translatedFullCircleFromQuarterArc )
import           WorldSize( WorldSize(..)
                          , Location(..)
                          , location
                          , reboundMaxRecurse )


data Animation = Animation {
    _animationNextTime :: !UTCTime
  , _animationCounter  :: !Int
  , _animationRender :: !(Animation -> WorldSize -> RenderState -> IO (Maybe Animation))
}

data AnimationProgress = AnimationInProgress
                       | AnimationDone
                       deriving(Eq, Show)

mkAnimation :: (Animation -> WorldSize -> RenderState -> IO (Maybe Animation))
            -> UTCTime
            -> Animation
mkAnimation render currentTime = Animation (addUTCTime animationPeriod currentTime) 0 render


simpleExplosionPure :: Coords -> Int -> [Coords]
simpleExplosionPure center iteration =
  let radius = fromIntegral iteration :: Float
      resolution = 8
  in translatedFullCircleFromQuarterArc center radius 0 resolution

quantitativeExplosionPure :: Int -> Coords -> Int -> [Coords]
quantitativeExplosionPure number center iteration =
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
  , _animationOriginFirstIteration :: !(Maybe Int)
}

-- | This function tests if the second animation has already begun. If it has begun, it continues it.
-- Else If first animation is outside the world, it starts second animation.
-- Else, it continues first animation.
firstAnimationWhileInsideWorldThenSecondAnimation :: (Coords -> Int -> [Coords])
                                                  -- ^ the second animation function (parameters are center and iteration)
                                                  -> WorldSize
                                                  -> Int
                                                  -> (Coords, AnimationOrigin)
                                                  -- ^ (first animation point, current second animation origin)
                                                  -> ([Coords], AnimationOrigin)
                                                  -- ^ (resulting animation points : either [first animation point] or results of the second animation
                                                  --    , possibly modified origin of second animation (if second animation has not started yet or has just started)))
firstAnimationWhileInsideWorldThenSecondAnimation anim2 sz iteration (anim1Point, col@(AnimationOrigin anim2Origin mayAnim2FirstIteration)) =
 case mayAnim2FirstIteration of
  (Just anim2FirstIteration) ->
  -- the second animation has started already
    let anim2Iteration = iteration - anim2FirstIteration
    in (anim2 anim2Origin anim2Iteration, col)
  Nothing -> case location anim1Point sz of
    OutsideWorld ->
    -- This is the first iteration where we are outside. anim1Point is the previous point
    -- which was set in the previous iteration, in the 'InsideWorld' match of this pattern match
    --
    -- Also, at the previous iteration the resulting point was equivalent to the call hereunder
    -- with a radius 0 so we use a radius 1 here and make sure to record the previous iteration
    -- as the start of the 2nd animation, so that at the next iteration the radius will increase (= 2).
      (anim2 anim2Origin 1, AnimationOrigin anim2Origin $ Just $ pred iteration)
    InsideWorld ->
    -- the first animation has not ended yet.
      ([anim1Point], AnimationOrigin anim1Point Nothing)


-- | In this function, 'Int' in the types of functions passed as arguments are iterations.
--   Coords are animation origins.
anim1WhileInsideWorldThenAnim2 :: (Coords -> Int -> [Coords])
                               -- ^ First animation. Regardless of iteration number, it should
                               -- returns the same number of points, and that 'same index' points
                               -- correlate across iterations.
                               -> (Coords -> Int -> [Coords])
                               -- ^ Second animation.
                               -> WorldSize
                               -> [AnimationOrigin]
                               -> Coords
                               -> Int
                               -> ([Coords], [AnimationOrigin])
anim1WhileInsideWorldThenAnim2 anim1 anim2 sz prevAnim2Origins center iteration =
  let anim1Points = anim1 center iteration
      anim2Origins =
        if null prevAnim2Origins
        then map (`AnimationOrigin` Nothing) anim1Points
        else prevAnim2Origins
      (pointsLists, newAnim2Origins) = unzip $
        map (firstAnimationWhileInsideWorldThenSecondAnimation anim2 sz iteration) $
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
stepAnimation (Animation t i f) = Animation (addUTCTime animationPeriod t) (succ i) f

earliestDeadline :: [Animation] -> Maybe UTCTime
earliestDeadline []         = Nothing
earliestDeadline animations = Just $ minimum $ map (\(Animation deadline _ _) -> deadline) animations


--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------


renderAnimations :: WorldSize -> RenderState -> [Animation] -> IO [Animation]
renderAnimations sz r anims = do
  newAnims <- mapM (\a@(Animation _ _ render) -> (render a sz r)) anims
  return $ catMaybes newAnims

renderCharIfInFrame :: Char -> Coords -> WorldSize -> RenderState -> IO Location
renderCharIfInFrame char pos sz (RenderState upperLeftCoords) = do
  let loc = location pos sz
  case loc of
    OutsideWorld -> return ()
    InsideWorld -> renderChar_ char $ RenderState $ sumCoords pos upperLeftCoords
  return loc

setRender :: Animation
          -> (Animation -> WorldSize -> RenderState -> IO (Maybe Animation))
          -> Animation
setRender (Animation t i _) = Animation t i

simpleExplosion :: Coords -> Animation -> WorldSize -> RenderState -> IO (Maybe Animation)
simpleExplosion center a@(Animation _ i _) = do
  let points = simpleExplosionPure center i
  renderAnimation points $ setRender a $ simpleExplosion center

quantitativeExplosion :: Int -> Coords -> Animation -> WorldSize -> RenderState -> IO (Maybe Animation)
quantitativeExplosion number center a@(Animation _ i _) sz = do
  let originalPoints = quantitativeExplosionPure number center i
      points = mapMaybe (reboundMaxRecurse sz 4) originalPoints
  renderAnimation points (setRender a $ quantitativeExplosion number center) sz

quantitativeExplosionThenSimpleExplosion :: [AnimationOrigin] -> Int -> Coords -> Animation -> WorldSize -> RenderState -> IO (Maybe Animation)
quantitativeExplosionThenSimpleExplosion anim2Origins number center a@(Animation _ i _) sz = do
  let (points, newAnim2Origins) = anim1WhileInsideWorldThenAnim2 (quantitativeExplosionPure number) simpleExplosionPure sz anim2Origins center i
  renderAnimation points (setRender a $ quantitativeExplosionThenSimpleExplosion newAnim2Origins number center) sz

renderAnimation :: [Coords] -> Animation -> WorldSize -> RenderState -> IO (Maybe Animation)
renderAnimation points a sz state = do
  loc <- renderPoints points '.' sz state
  return $ if loc == OutsideWorld then Nothing else Just a

renderPoints :: [Coords] -> Char -> WorldSize -> RenderState -> IO Location
renderPoints points char sz state = do
  locations <- mapM (\c -> renderCharIfInFrame char c sz state) points
  return $ if null locations || all (== OutsideWorld) locations then OutsideWorld else InsideWorld
