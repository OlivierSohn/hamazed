
module Animation
    ( Animation(..)
    , mkAnimation
    , stepClosest
    , earliestAnimationTime
    , drawPoint
    , explosion
    , renderAnimations
    , WorldSize(..)
    ) where


import           Control.Monad( filterM )
import           Data.List( partition )
import           Data.Time( addUTCTime
                          , NominalDiffTime
                          , UTCTime )

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
  , _animationRender :: Int -> WorldSize -> RenderState -> IO AnimationProgress
}

mkAnimation :: (Int -> WorldSize -> RenderState -> IO AnimationProgress)
            -> UTCTime
            -> Animation
mkAnimation render currentTime = Animation (addUTCTime animationPeriod currentTime) 0 render

animationIsOver :: Animation -> Bool
animationIsOver (Animation _ i _) = i == 60 -- TODO make it parametrable

animationPeriod :: Data.Time.NominalDiffTime
animationPeriod = 0.05

timeOf :: Animation -> UTCTime
timeOf (Animation t _ _) = t

-- steps the animations which will be done the soonest
stepClosest :: [Animation] -> [Animation]
stepClosest [] = error "should never happen"
stepClosest l = let m = minimum $ map timeOf l
                    (closest, other) = partition (\a -> timeOf a == m) l
                in other ++ filter (not . animationIsOver) (map stepAnimation closest)

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
  filterM (\(Animation _ i render) -> ((==) AnimationInProgress <$> render i sz r))


makePoint :: Int -> Coords
makePoint i = Coords (Row 0) (Col i)

drawPoint :: Int -> WorldSize -> RenderState -> IO ()
drawPoint i =
  renderCharIfInFrame '.' (makePoint i)

renderCharIfInFrame :: Char -> Coords -> WorldSize -> RenderState -> IO ()
renderCharIfInFrame char pos sz (RenderState upperLeftCoords) =
  case location pos sz of
    OutsideWorld -> return ()
    InsideWorld -> renderChar_ char $ RenderState $ sumCoords pos upperLeftCoords

makePointOnCircle :: Float -> Float -> Coords
makePointOnCircle radius angle =
  let x = radius * sin angle
      y = radius * cos angle
      toInt flt = floor $ 0.5 + flt
  in Coords (Row $ toInt y) (Col $ toInt x)

discretizeArcOfCircle :: Float -> Float -> [Coords]
discretizeArcOfCircle radius arcAngle =
  let resolution = 8 :: Integer
      angleIncrement = arcAngle / (fromIntegral resolution :: Float)
  in  map (\i ->
        let angle = angleIncrement * (fromIntegral i :: Float)
        in makePointOnCircle radius angle) [0..resolution]

explosion :: Int -> Coords -> Int -> WorldSize -> RenderState -> IO AnimationProgress
explosion _ center iteration sz state = do
  let radius = fromIntegral iteration :: Float
      quarterCircle = discretizeArcOfCircle radius (pi/2)
      circle = map (sumCoords center) $ concatMap rotateByQuarters quarterCircle
  mapM_ (\c -> renderCharIfInFrame '.' c sz state) circle
  if iteration > 20 then return AnimationDone else return AnimationInProgress
