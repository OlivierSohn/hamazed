
module World
    ( Action(..)
    , ActionTarget(..)
    , Animation(..)
    , mkAnimation
    , BattleShip(..)
    , accelerateShip
    , coordsForActionTargets
    , extend
    , mkWorld
    , moveWorld
    , nextWorld
    , earliestAnimationDeadline
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
                          , earliestDeadline
                          , stepEarliest )
import           Geo( Col(..)
                    , Coords(..)
                    , coordsForDirection
                    , translateInDir
                    , Direction(..)
                    , PosSpeed(..)
                    , Row(..)
                    , sumCoords
                    , zeroCoords )
import           Space( Space(..)
                      , getMaterial
                      , Material(..)
                      , mkRectangle )
import           WorldSize( WorldSize(..) )

data Action = Action ActionTarget Direction
            | Timeout Step
            | StartLevel Int
            | EndGame
            | Nonsense
            deriving(Eq, Show)

data Step = GameStep
          | AnimationStep
          | MessageStep
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


extend :: Coords -> Direction -> Space -> Coords
extend coords dir space =
  let extended = translateInDir dir coords
  in case getMaterial extended space of
    Wall -> coords
    Air -> extend extended dir space


data BattleShip = BattleShip {
    _shipPosSpeed :: !PosSpeed
  , _shipAmmo :: !Int
  , _shipSafeUntil :: !(Maybe UTCTime)
  , _shipCollisions :: ![Number]
}

accelerateShip :: Direction -> BattleShip -> BattleShip
accelerateShip dir (BattleShip (PosSpeed pos speed) ba bb bc) =
  let newSpeed = sumCoords speed $ coordsForDirection dir
  in BattleShip (PosSpeed pos newSpeed) ba bb bc

data World = World{
    _worldNumbers :: ![Number]
  , _howBallMoves :: Space -> PosSpeed -> PosSpeed
  , _worldShip :: !BattleShip
  , _worldSpace :: !Space
  , _worldAnimations :: ![Animation]
}

data Number = Number {
    _numberPosSpeed :: !PosSpeed
  , _numberNum :: !Int
}

getColliding :: Coords -> [Number] -> [Number]
getColliding pos = filter (\(Number (PosSpeed pos' _) _) -> pos == pos')

nextWorld :: World -> [Number] -> Int -> [Animation] -> World
nextWorld (World _ changePos (BattleShip posspeed _ safeTime collisions) size _) balls ammo =
  World balls changePos (BattleShip posspeed ammo safeTime collisions) size

-- move the world elements (numbers, ship), but do NOT advance the animations
moveWorld :: UTCTime -> World -> World
moveWorld curTime (World balls changePos (BattleShip shipPosSpeed ammo safeTime _) size anims) =
  let newSafeTime = case safeTime of
        (Just t) -> if curTime > t then Nothing else safeTime
        _        -> Nothing
      newBalls = map (\(Number ps n) -> Number (changePos size ps) n) balls
      newPosSpeed@(PosSpeed pos _) = changePos size shipPosSpeed
      collisions = getColliding pos newBalls
      newShip = BattleShip newPosSpeed ammo newSafeTime collisions
  in World newBalls changePos newShip size anims

ballMotion :: Space -> PosSpeed -> PosSpeed
ballMotion worldSize = doBallMotion . mirrorIfNeeded worldSize

doBallMotion :: PosSpeed -> PosSpeed
doBallMotion (PosSpeed (Coords (Row r) (Col c)) (Coords (Row dr) (Col dc))) =
    PosSpeed (Coords (Row newR) (Col newC)) (Coords (Row dr) (Col dc))
  where
    newR = r + dr
    newC = c + dc

-- when continuing with current speed, if at next iteration we encounter a wall,
-- then we change the speed now according to the wall normal
mirrorIfNeeded :: Space -> PosSpeed -> PosSpeed
mirrorIfNeeded space (PosSpeed (Coords (Row r) (Col c)) (Coords (Row dr) (Col dc))) =
  let future = Coords (Row $ r+dr) (Col $ c+dc)
      isWall coord = getMaterial coord space == Wall
      (newDr, newDc) = if isWall future
        then
          if dr == 0
            then (0, negate dc)
            else
              if dc == 0
              then (negate dr, 0)
              else
                -- diagonal case
                (if isWall (Coords (Row $ r+dr) (Col c)) then negate dr else dr,
                  if isWall (Coords (Row r) (Col $ c+dc)) then negate dc else dc)
        else (dr, dc)
      -- we chose to not constrain the positions as it leads to unnatural motions
      -- the tradeoff is just to not render them
  in PosSpeed (Coords (Row r) (Col c)) (Coords (Row newDr) (Col newDc))

stepEarliestAnimations :: World -> World
stepEarliestAnimations (World a b c d animations) = World a b c d (stepEarliest animations)

earliestAnimationDeadline :: World -> Maybe UTCTime
earliestAnimationDeadline (World _ _ _ _ animations) = earliestDeadline animations

--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

mkWorld :: WorldSize -> [Int] -> IO World
mkWorld (WorldSize (Coords r c)) nums = do
  let space = mkRectangle r c
  balls <- mapM (createRandomNumber space) nums
  ship@(PosSpeed pos _) <- createRandomPosSpeed space
  t <- getCurrentTime
  return $ World balls ballMotion (BattleShip ship 10 (Just $ addUTCTime 5 t) (getColliding pos balls)) space []

randomInt :: Int -> IO Int
randomInt sz = getStdRandom $ randomR (0,sz-1)

randomCoords :: WorldSize -> IO Coords
randomCoords (WorldSize (Coords rs cs)) = do
  r <- randomRow rs
  c <- randomCol cs
  return $ Coords r c

randomRow :: Row -> IO Row
randomRow (Row sz) = Row <$> randomInt sz

randomCol :: Col -> IO Col
randomCol (Col sz) = Col <$> randomInt sz

randomSpeed :: IO Int
randomSpeed = getStdRandom $ randomR (-1,1)

createRandomPosSpeed :: Space -> IO PosSpeed
createRandomPosSpeed space = do
  pos <- randomNonCollidingPos space
  dx <- randomSpeed
  dy <- randomSpeed
  return $ mirrorIfNeeded space $ PosSpeed pos (Coords (Row dx) (Col dy))

randomNonCollidingPos :: Space -> IO Coords
randomNonCollidingPos space@(Space _ worldSize _) = do
  coords <- randomCoords worldSize
  case getMaterial coords space of
    Wall -> randomNonCollidingPos space
    Air -> return coords

createRandomNumber :: Space -> Int -> IO Number
createRandomNumber space i = do
  ps <- createRandomPosSpeed space
  return $ Number ps i
