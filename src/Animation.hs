
module Animation
    ( Animation(..)
    , mkAnimation
    , stepClosest
    , earliestAnimationTime
    , drawPoint
    , simpleExplosion
    , quantitativeExplosion
    , renderAnimations
    , WorldSize(..)
    ) where


import           Control.Monad( filterM )

import           Data.List( partition )
import           Data.Time( addUTCTime
                          , NominalDiffTime
                          , UTCTime )
import           Data.Maybe(isJust)
import           System.Random( getStdRandom
                              , randomR )


import           Console( RenderState(..)
                        , renderChar_ )
import           Geo( Col(..)
                    , Coords(..)
                    , Row(..)
                    , rotateByQuarters
                    , sumCoords )
import           WorldSize( WorldSize(..)
                          , Location(..)
                          , location )

data AnimationProgress = AnimationInProgress
                       | AnimationDone
                       deriving(Eq, Show)

data Animation = Animation {
    _animationNextTime :: !UTCTime
  , _animationCounter  :: !Int
  , _animationRender :: Animation -> WorldSize -> RenderState -> IO (Maybe Animation)
}

mkAnimation :: (Animation -> WorldSize -> RenderState -> IO (Maybe Animation))
            -> UTCTime
            -> Animation
mkAnimation render currentTime = Animation (addUTCTime animationPeriod currentTime) 0 render

animationPeriod :: Data.Time.NominalDiffTime
animationPeriod = 0.02

timeOf :: Animation -> UTCTime
timeOf (Animation t _ _) = t

-- steps the animations which will be done the soonest
stepClosest :: [Animation] -> [Animation]
stepClosest [] = error "should never happen"
stepClosest l = let m = minimum $ map timeOf l
                    (closest, other) = partition (\a -> timeOf a == m) l
                in other ++ map stepAnimation closest

stepAnimation :: Animation -> Animation
stepAnimation (Animation t i f) = Animation (addUTCTime animationPeriod t) (succ i) f

earliestAnimationTime :: [Animation] -> Maybe UTCTime
earliestAnimationTime []         = Nothing
earliestAnimationTime animations = Just $ minimum $ map timeOf animations


--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------


renderAnimations :: WorldSize -> RenderState -> [Animation] -> IO [Animation]
renderAnimations sz r =
  filterM (\a@(Animation _ _ render) -> (isJust <$> render a sz r))


makePoint :: Int -> Coords
makePoint i = Coords (Row 0) (Col i)

drawPoint :: Int -> WorldSize -> RenderState -> IO Location
drawPoint i =
  renderCharIfInFrame '.' (makePoint i)

renderCharIfInFrame :: Char -> Coords -> WorldSize -> RenderState -> IO Location
renderCharIfInFrame char pos sz (RenderState upperLeftCoords) = do
  let loc = location pos sz
  case loc of
    OutsideWorld -> return ()
    InsideWorld -> renderChar_ char $ RenderState $ sumCoords pos upperLeftCoords
  return loc

makePointOnCircle :: Float -> Float -> Coords
makePointOnCircle radius angle =
  let x = radius * sin angle
      y = radius * cos angle
      toInt flt = floor $ 0.5 + flt
  in Coords (Row $ toInt y) (Col $ toInt x)

discretizeArcOfCircle :: Float -> Float -> Float -> Int -> [Coords]
discretizeArcOfCircle radius arcAngle firstAngle resolution =
  let angleIncrement = arcAngle / (fromIntegral resolution :: Float)
  in  map (\i ->
        let angle = firstAngle + angleIncrement * (fromIntegral i :: Float)
        in makePointOnCircle radius angle) [0..resolution]

fullCircleFromQuarterArc :: Float -> Int -> [Coords]
fullCircleFromQuarterArc radius quarterArcResolution =
  let quarterArcAngle = pi/2
      quarterCircle = discretizeArcOfCircle radius quarterArcAngle 0.0 quarterArcResolution
  in  concatMap rotateByQuarters quarterCircle

fullCircle :: Float -> Float -> Int -> [Coords]
fullCircle radius firstAngle resolution =
  let totalAngle = 2*pi
  in  discretizeArcOfCircle radius totalAngle firstAngle resolution


simpleExplosion :: Coords -> Animation -> WorldSize -> RenderState -> IO (Maybe Animation)
simpleExplosion center a@(Animation _ iteration _) sz state = do
  let radius = fromIntegral iteration :: Float
      resolution = 8
      circle = fullCircleFromQuarterArc radius resolution
      translatedCircle = map (sumCoords center) circle
  locations <- mapM (\c -> renderCharIfInFrame '.' c sz state) translatedCircle
  return $ if all (== OutsideWorld) locations then Nothing else Just a


quantitativeExplosion :: Int -> Coords -> Animation -> WorldSize -> RenderState -> IO (Maybe Animation)
quantitativeExplosion number center a@(Animation _ iteration _) sz state = do
  let numRand = 10 :: Int
      rnd = 2 :: Int -- TODO store the random number in the state of the animation
  -- rnd <- getStdRandom $ randomR (0,numRand-1)
  let radius = fromIntegral iteration :: Float
      firstAngle = (fromIntegral rnd :: Float) * 2*pi / (fromIntegral numRand :: Float)
      circle = fullCircle radius firstAngle number
      translatedCircle = map (sumCoords center) circle
  locations <- mapM (\c -> renderCharIfInFrame '.' c sz state) translatedCircle
  return $ if all (== OutsideWorld) locations then Nothing else Just a
