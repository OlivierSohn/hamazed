module World
    ( Animation(..)
    , mkAnimation
    , BattleShip(..)
    , accelerateShip
    , World(..)
    , mkWorld
    , moveWorld
    , nextWorld
    , renderWorld
    , earliestAnimationDeadline
    -- | reexports
    , Number(..)
    ) where

import           Imajuscule.Prelude

import           Data.Char( intToDigit )
import           Data.Maybe( isNothing )
import           Data.Time( addUTCTime
                          , getCurrentTime
                          , UTCTime )
import           System.Random( getStdRandom
                              , randomR )


import           Animation( Animation(..)
                          , mkAnimation
                          , earliestDeadline )
import           Console( ColorIntensity(..)
                        , Color(..)
                        , setForeground )
import           Geo( Col(..)
                    , Coords(..)
                    , coordsForDirection
                    , Direction(..)
                    , PosSpeed(..)
                    , Row(..)
                    , sumCoords )
import           Number( Number(..)
                       , getColliding )
import           Render( RenderState )
import           Space( Space(..)
                      , renderIfNotColliding
                      , getMaterial
                      , Material(..)
                      , mkRandomlyFilledSpace )
import           Timing( KeyTime(..) )
import           WorldSize( WorldSize(..) )



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
      (newDr, newDc) = case getMaterial future space of
        Wall
          | dr == 0   -> (0, negate dc)
          | dc == 0   -> (negate dr, 0)
          | otherwise -> -- diagonal case
                case (isWall (Coords (Row $ r+dr) (Col c)),
                      isWall (Coords (Row r) (Col $ c+dc))) of
                        (True, True)   -> (negate dr, negate dc)
                        (False, False) -> (negate dr, negate dc)
                        (True, False)  -> (negate dr, dc)
                        (False, True)  -> (dr, negate dc)
        Air
          | dr == 0   -> (dr, dc)
          | dc == 0   -> (dr, dc)
          | otherwise -> -- diagonal case
                case (isWall (Coords (Row $ r+dr) (Col c)),
                      isWall (Coords (Row r) (Col $ c+dc))) of
                        (True, True)   -> (negate dr, negate dc)
                        (False, False) -> (dr, dc)
                        (True, False)  -> (negate dr, dc)
                        (False, True)  -> (dr, negate dc)

      -- we chose to not constrain the positions as it leads to unnatural motions
      -- the tradeoff is just to not render them
  in PosSpeed (Coords (Row r) (Col c)) (Coords (Row newDr) (Col newDc))

earliestAnimationDeadline :: World -> Maybe KeyTime
earliestAnimationDeadline (World _ _ _ _ animations) = earliestDeadline animations

--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

mkWorld :: WorldSize -> [Int] -> IO World
mkWorld s nums = do
  space <- mkRandomlyFilledSpace s
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

renderWorld :: World -> RenderState -> IO ()
renderWorld (World balls _ (BattleShip (PosSpeed shipCoords _) _ safeTime collisions) space _) worldCorner = do
  -- render numbers, including the ones that will be destroyed, if any
  mapM_ (\(Number (PosSpeed pos _) i) -> renderIfNotColliding (intToDigit i) pos space worldCorner) balls
  when (null collisions) (do
    let shipColor = if isNothing safeTime then Blue else Red
    setForeground Vivid shipColor
    renderIfNotColliding '+' shipCoords space worldCorner
    setForeground Vivid White)
