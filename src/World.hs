{-# LANGUAGE DeriveGeneric #-}

module World
    ( Action(..)
    , ActionTarget(..)
    , actionFromChar
    , Animation(..)
    , renderAnimations
    , BattleShip(..)
    , shipCollides
    , coordsForActionTargets
    , extend
    , Location(..)
    , location
    , mkWorld
    , moveWorld
    , nextWorld
    , earliestAnimationTimeInWorld
    , stepEarliestAnimations
    , Number(..)
    , Step(..)
    , World(..)
    , animationIsOver
    , WorldSize(..)
    ) where


import           Data.List( foldl'
                          , partition )
import           Data.Maybe( mapMaybe )
import           Data.Time( addUTCTime
                          , getCurrentTime
                          , NominalDiffTime
                          , UTCTime )
import           GHC.Generics( Generic )
import           System.Random( getStdRandom
                              , randomR )


import           Console( RenderState(..)
                        , renderChar_ )
import           Geo( Col(..)
                    , Coords(..)
                    , coordsForDirection
                    , Direction(..)
                    , PosSpeed(..)
                    , Row(..)
                    , sumCoords
                    , zeroCoords )

newtype WorldSize = WorldSize { _worldSizeValue :: Int } deriving(Generic, Eq, Show)

data Action = Action ActionTarget Direction |
              Timeout Step |
              Nonsense
              deriving(Eq, Show)

data Step = WorldStep
          | AnimationStep
          deriving(Eq, Show)

data ActionTarget = Frame
                  | Ship
                  | Laser
                  deriving(Eq, Show)

data Location = InsideWorld | OutsideWorld


coordsForActionTargets :: ActionTarget -> [Action] -> Coords
coordsForActionTargets target actions = foldl' sumCoords zeroCoords $ map coordsForDirection $ filterActions target actions

filterActions :: ActionTarget -> [Action] -> [Direction]
filterActions target = mapMaybe (maybeDirectionFor target)

maybeDirectionFor :: ActionTarget -> Action -> Maybe Direction
maybeDirectionFor targetFilter (Action actionTarget dir)
   | actionTarget == targetFilter = Just dir
   | otherwise                    = Nothing
maybeDirectionFor _ _ = Nothing


actionFromChar :: Char -> Action
actionFromChar c = case c of
  'g' -> Action Frame Down
  't' -> Action Frame Up
  'f' -> Action Frame LEFT
  'h' -> Action Frame RIGHT
  'k' -> Action Laser Down
  'i' -> Action Laser Up
  'j' -> Action Laser LEFT
  'l' -> Action Laser RIGHT
  's' -> Action Ship Down
  'w' -> Action Ship Up
  'a' -> Action Ship LEFT
  'd' -> Action Ship RIGHT
  _   -> Nonsense


location :: Coords -> WorldSize -> Location
location (Coords (Row r) (Col c)) (WorldSize worldSize)
  | inside r && inside c = InsideWorld
  | otherwise            = OutsideWorld
  where inside x = x >= 0 && x < worldSize


extend :: Coords -> Direction -> WorldSize -> Coords
extend (Coords (Row r) (Col c)) dir (WorldSize worldSize) = case dir of
  Up    -> Coords (Row 0) (Col c)
  LEFT  -> Coords (Row r) (Col 0)
  Down  -> Coords (Row (worldSize-1)) (Col c)
  RIGHT -> Coords (Row r) (Col (worldSize-1))


data BattleShip = BattleShip {
    _shipPosSpeed :: !PosSpeed
  , _shipAmmo :: !Int
  , _shipSafeUntil :: !(Maybe UTCTime)
}

data World = World{
    _worldNumber :: ![Number]
  , _howBallMoves :: WorldSize -> PosSpeed -> PosSpeed
  , _worldShip :: !BattleShip
  , _worldWorldSize :: !WorldSize
  , _worldAnimations :: ![Animation]
}

data Number = Number {
    _numberPosSpeed :: !PosSpeed
  , _numberNum :: !Int
}

data Animation = Animation {
    _animationNextTime :: !UTCTime
  , _animationCounter  :: !Int
  , _animationRender :: Int -> RenderState -> IO ()
}

animationIsOver :: Animation -> Bool
animationIsOver (Animation _ i _) = i == 60

shipCollides :: World -> [Number]
shipCollides (World balls _ (BattleShip (PosSpeed shipCoords _) _ _) _ _) =
   filter (\(Number (PosSpeed pos _) _) -> shipCoords == pos) balls

nextWorld :: Action -> World -> [Number] -> Int -> World
nextWorld action (World _ changePos (BattleShip (PosSpeed shipPos shipSpeed) _ safeTime) size anims) balls ammo =
  let shipAcceleration = coordsForActionTargets Ship [action]
      shipSamePosChangedSpeed = PosSpeed shipPos $ sumCoords shipSpeed shipAcceleration
  in World balls changePos (BattleShip shipSamePosChangedSpeed ammo safeTime) size anims

-- move the world elements (numbers, ship), but do NOT advance the animations
moveWorld :: UTCTime -> World -> World
moveWorld curTime (World balls changePos (BattleShip shipPosSpeed ammo safeTime) size anims) =
  let newSafeTime = case safeTime of
        (Just t) -> if curTime > t then Nothing else safeTime
        _        -> Nothing
      newBalls = map (\(Number ps n) -> Number (changePos size ps) n) balls
      newShip = BattleShip (changePos size shipPosSpeed) ammo newSafeTime
  in World newBalls changePos newShip size anims

ballMotion :: WorldSize -> PosSpeed -> PosSpeed
ballMotion worldSize = doBallMotion . mirrorIfNeeded worldSize

doBallMotion :: PosSpeed -> PosSpeed
doBallMotion (PosSpeed (Coords (Row r) (Col c)) (Coords (Row dr) (Col dc))) =
    PosSpeed (Coords (Row newR) (Col newC)) (Coords (Row dr) (Col dc))
  where
    newR = r + dr
    newC = c + dc

mirrorSpeedIfNeeded :: WorldSize -> Int -> Int -> Int
mirrorSpeedIfNeeded (WorldSize worldSize) x
  | x <= 0           = abs
  | x >= worldSize-1 = negate . abs
  | otherwise        = id

{--
constrainPos :: Int -> Int
constrainPos r
  | r < 0 = negate r
  | r >= worldSize = 2 * worldSize - r
  | otherwise = r

constrainPosSticky :: Int -> Int
constrainPosSticky r
  | r < 0 = 0
  | r >= worldSize = worldSize - 1
  | otherwise = r
--}

mirrorIfNeeded :: WorldSize -> PosSpeed -> PosSpeed
mirrorIfNeeded worldSize (PosSpeed (Coords (Row r) (Col c)) (Coords (Row dr) (Col dc))) =
  let newDr = mirrorSpeedIfNeeded worldSize r dr
      newDc = mirrorSpeedIfNeeded worldSize c dc
      -- we chose to not constrain the positions as it leads to unnatural motions
      -- the tradeoff is just to not render them
      newR = r
      newC = c
  in PosSpeed (Coords (Row newR) (Col newC)) (Coords (Row newDr) (Col newDc))

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

stepEarliestAnimations :: World -> World
stepEarliestAnimations (World a b c d animations) = World a b c d (stepClosest animations)

earliestAnimationTimeInWorld :: World -> Maybe UTCTime
earliestAnimationTimeInWorld (World _ _ _ _ animations) = earliestAnimationTime animations

earliestAnimationTime :: [Animation] -> Maybe UTCTime
earliestAnimationTime []         = Nothing
earliestAnimationTime animations = Just $ minimum $ map timeOf animations

--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

drawPoint :: Int -> RenderState -> IO ()
drawPoint i (RenderState upperLeftCoords) = do
  let pos = Coords (Row 0) (Col i)
  renderChar_ '.' $ RenderState $ sumCoords pos upperLeftCoords


renderAnimations :: RenderState -> [Animation] -> IO ()
renderAnimations r = mapM_ (\(Animation _ i render) -> render i r)

mkWorld :: WorldSize -> [Int] -> IO World
mkWorld worldSize nums = do
  balls <- mapM (createRandomNumber worldSize) nums
  ship <- createRandomPosSpeed worldSize
  t <- getCurrentTime
  return $ World balls ballMotion (BattleShip ship 10 (Just $ addUTCTime 5 t)) worldSize [Animation (addUTCTime animationPeriod t) 0 drawPoint]

randomPos :: WorldSize -> IO Int
randomPos (WorldSize worldSize) = getStdRandom $ randomR (0,worldSize-1)

randomSpeed :: IO Int
randomSpeed = getStdRandom $ randomR (-1,1)

createRandomPosSpeed :: WorldSize -> IO PosSpeed
createRandomPosSpeed worldSize = do
  x <- randomPos worldSize
  y <- randomPos worldSize
  dx <- randomSpeed
  dy <- randomSpeed
  return $ mirrorIfNeeded worldSize $ PosSpeed (Coords (Row x) (Col y)) (Coords (Row dx) (Col dy))

createRandomNumber :: WorldSize -> Int -> IO Number
createRandomNumber worldSize i = do
  ps <- createRandomPosSpeed worldSize
  return $ Number ps i
