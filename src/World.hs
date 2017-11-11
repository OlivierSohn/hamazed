
module World
    ( Action(..)
    , ActionTarget(..)
    , actionFromChar
    , BattleShip(..)
    , shipCollides
    , coordsForActionTargets
    , extend
    , Location(..)
    , location
    , mkWorld
    , moveWorld
    , nextWorld
    , Number(..)
    , World(..)
    , worldSize
    ) where


import           Data.List( foldl' )
import           Data.Maybe( mapMaybe )
import           System.Random( getStdRandom
                              , randomR )

import           Geo( Col(..)
                    , Coords(..)
                    , coordsForDirection
                    , Direction(..)
                    , PosSpeed(..)
                    , Row(..)
                    , sumCoords
                    , zeroCoords )

data Action = Action ActionTarget Direction |
              Timeout |
              Nonsense
              deriving (Show)

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
   | otherwise = Nothing
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


worldSize :: Int
worldSize = 35


location :: Coords -> Location
location (Coords (Row r) (Col c))
  | inside r && inside c = InsideWorld
  | otherwise            = OutsideWorld
  where inside x = x >= 0 && x < worldSize


extend :: Coords -> Direction -> Coords
extend (Coords (Row r) (Col c)) dir = case dir of
  Up    -> Coords (Row 0) (Col c)
  LEFT  -> Coords (Row r) (Col 0)
  Down  -> Coords (Row (worldSize-1)) (Col c)
  RIGHT -> Coords (Row r) (Col (worldSize-1))


data BattleShip = BattleShip {
    _shipPosSpeed :: !PosSpeed
  , _shipAmmo :: !Int
}
data World = World{
    _worldNumber :: ![Number]
  , _howBallMoves :: PosSpeed -> PosSpeed
  , _worldShip :: !BattleShip
}
data Number = Number {
    _numberPosSpeed :: !PosSpeed
  , _numberNum :: !Int
}


shipCollides :: World -> Bool
shipCollides (World balls _ (BattleShip (PosSpeed shipCoords _) _)) =
   any (\(Number (PosSpeed pos _) _) -> shipCoords == pos) balls

nextWorld :: Action -> World -> [Number] -> Int -> World
nextWorld action (World _ changePos (BattleShip (PosSpeed shipPos shipSpeed) _)) balls ammo =
  let shipAcceleration = coordsForActionTargets Ship [action]
      shipSamePosChangedSpeed = PosSpeed shipPos $ sumCoords shipSpeed shipAcceleration
  in World balls changePos $ BattleShip shipSamePosChangedSpeed ammo

moveWorld :: World -> World
moveWorld (World balls changePos (BattleShip ship ammo)) =
  World (map (\(Number ps n) -> Number (changePos ps) n) balls) changePos $ BattleShip (changePos ship) ammo

ballMotion :: PosSpeed -> PosSpeed
ballMotion = doBallMotion . mirrorIfNeeded

doBallMotion :: PosSpeed -> PosSpeed
doBallMotion (PosSpeed (Coords (Row r) (Col c)) (Coords (Row dr) (Col dc))) =
    PosSpeed (Coords (Row newR) (Col newC)) (Coords (Row dr) (Col dc))
  where
    newR = r + dr
    newC = c + dc


mirrorSpeedIfNeeded :: Int -> Int -> Int
mirrorSpeedIfNeeded x
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

mirrorIfNeeded :: PosSpeed -> PosSpeed
mirrorIfNeeded (PosSpeed (Coords (Row r) (Col c)) (Coords (Row dr) (Col dc))) =
  let newDr = mirrorSpeedIfNeeded r dr
      newDc = mirrorSpeedIfNeeded c dc
      -- we chose to not constrain the positions as it leads to unnatural motions
      -- the tradeoff is just to not render them
      newR = r
      newC = c

  in PosSpeed (Coords (Row newR) (Col newC)) (Coords (Row newDr) (Col newDc))


--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

mkWorld :: IO World
mkWorld = do
  balls <- mapM createRandomNumber [0..9]
  ship <- createRandomPosSpeed
  return (World balls ballMotion (BattleShip ship 10))


randomPos :: IO Int
randomPos = getStdRandom $ randomR (0,worldSize-1)


randomSpeed :: IO Int
randomSpeed = getStdRandom $ randomR (-1,1)


createRandomPosSpeed :: IO PosSpeed
createRandomPosSpeed = do
  x <- randomPos
  y <- randomPos
  dx <- randomSpeed
  dy <- randomSpeed
  return $ mirrorIfNeeded $ PosSpeed (Coords (Row x) (Col y)) (Coords (Row dx) (Col dy))


createRandomNumber :: Int -> IO Number
createRandomNumber i = do
  ps <- createRandomPosSpeed
  return $ Number ps i
