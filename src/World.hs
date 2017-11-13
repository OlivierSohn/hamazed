
module World
    ( Action(..)
    , ActionTarget(..)
    , actionFromChar
    , Animation(..)
    , mkAnimation
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
    ) where


import           Data.List( foldl' )
import           Data.Maybe( mapMaybe )
import           Data.Time( addUTCTime
                          , getCurrentTime
                          , UTCTime )
import           System.Random( getStdRandom
                              , randomR )


import           Animation( Animation(..)
                          , mkAnimation
                          , stepClosest
                          , earliestAnimationTime )
import           Geo( Col(..)
                    , Coords(..)
                    , coordsForDirection
                    , Direction(..)
                    , PosSpeed(..)
                    , Row(..)
                    , sumCoords
                    , zeroCoords )
import           WorldSize( WorldSize(..)
                          , Location(..)
                          , location )

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

shipCollides :: World -> [Number]
shipCollides (World balls _ (BattleShip (PosSpeed shipCoords _) _ _) _ _) =
   filter (\(Number (PosSpeed pos _) _) -> shipCoords == pos) balls

nextWorld :: Action -> World -> [Number] -> Int -> [Animation] -> World
nextWorld action (World _ changePos (BattleShip (PosSpeed shipPos shipSpeed) _ safeTime) size _) balls ammo anims =
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

stepEarliestAnimations :: World -> World
stepEarliestAnimations (World a b c d animations) = World a b c d (stepClosest animations)

earliestAnimationTimeInWorld :: World -> Maybe UTCTime
earliestAnimationTimeInWorld (World _ _ _ _ animations) = earliestAnimationTime animations

--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

mkWorld :: WorldSize -> [Int] -> IO World
mkWorld worldSize nums = do
  balls <- mapM (createRandomNumber worldSize) nums
  ship <- createRandomPosSpeed worldSize
  t <- getCurrentTime
  return $ World balls ballMotion (BattleShip ship 10 (Just $ addUTCTime 5 t)) worldSize []

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
