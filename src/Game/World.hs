{-# LANGUAGE NoImplicitPrelude #-}

module Game.World
    ( BattleShip(..)
    , accelerateShip
    , World(..)
    , BoundedAnimation(..)
    , Boundaries(..)
    , mkWorld
    , moveWorld
    , nextWorld
    , renderWorld
    , earliestAnimationDeadline
    , earliestFrameAnimationDeadline
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


import           Animation.Types
import           Animation.Util( earliestDeadline )

import           Collision

import           Color

import           Geo.Discrete.Bresenham
import           Geo.Discrete
import           Game.World.Size

import           Game.World.Frame
import           Game.World.Number
import           Game.World.Space

import           Render( RenderState )
import           Render.Console

import           Timing


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
  , _worldAnimations :: ![BoundedAnimation]
}

data BoundedAnimation = BoundedAnimation Animation Boundaries

data Boundaries = WorldFrame
                | TerminalWindow
                | Both

nextWorld :: World -> [Number] -> Int -> [BoundedAnimation] -> World
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
ballMotion space ps@(PosSpeed pos _) =
  let (newPs@(PosSpeed newPos _), collision) = mirrorIfNeeded (`location` space) ps
  in  case collision of
        PreCollision ->
          if pos /= newPos
            then
              newPs
            else
              -- Precollision position is the same as the previous position, we try to move
              doBallMotionUntilCollision space newPs
        NoCollision  -> doBallMotion newPs

doBallMotion :: PosSpeed -> PosSpeed
doBallMotion (PosSpeed pos speed) =
  PosSpeed (sumCoords pos speed) speed

-- | Changes the position until a collision is found.
--   Doesn't change the speed
doBallMotionUntilCollision :: Space -> PosSpeed -> PosSpeed
doBallMotionUntilCollision space (PosSpeed pos speed) =
  let trajectory = bresenham $ mkSegment pos $ sumCoords pos speed
      newPos = maybe (last trajectory) snd $ firstCollision (`location` space) trajectory
  in PosSpeed newPos speed

earliestAnimationDeadline :: World -> Maybe KeyTime
earliestAnimationDeadline (World _ _ _ _ animations) =
  earliestDeadline $ map (\(BoundedAnimation a _) -> a) animations

earliestFrameAnimationDeadline :: World -> Maybe KeyTime
earliestFrameAnimationDeadline (World _ _ _ (Space _ mayAnim _ _) _) = 
  maybe Nothing (\(FrameAnimation _ _ deadline) -> Just deadline) mayAnim

--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

mkWorld :: Maybe WorldSize -> WorldSize -> WallType -> [Int] -> IO World
mkWorld mayPrevSize s walltype nums = do
  (Space mat _ sz render) <- case walltype of
    None          -> return $ mkEmptySpace s
    Deterministic -> return $ mkDeterministicallyFilledSpace s
    Random rParams    -> mkRandomlyFilledSpace rParams s
  t <- getCurrentTime
  let frameAnimation =
        maybe
          Nothing
          (\prev -> Just $ FrameAnimation prev 0 $ addFrameAnimationStepDuration $ KeyTime t)
            mayPrevSize
      space = Space mat frameAnimation sz render
  balls <- mapM (createRandomNumber space) nums
  ship@(PosSpeed pos _) <- createShipPos space balls
  return $ World balls ballMotion (BattleShip ship 10 (Just $ addUTCTime 5 t) (getColliding pos balls)) space []

createShipPos :: Space -> [Number] -> IO PosSpeed
createShipPos space numbers = do
  let numPositions = map (\(Number (PosSpeed pos _) _) -> pos) numbers
  candidate@(PosSpeed pos _) <- createRandomPosSpeed space
  if pos `notElem` numPositions
    then
      return candidate
    else
      createShipPos space numbers

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
  return $ fst $ mirrorIfNeeded (`location` space) $ PosSpeed pos (Coords (Row dx) (Col dy))

randomNonCollidingPos :: Space -> IO Coords
randomNonCollidingPos space@(Space _ _ worldSize _) = do
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
  mapM_ (\b -> renderNumber b space worldCorner) balls
  when (null collisions) (do
    let colors = if isNothing safeTime then shipColors else shipColorsSafe
    c <- setColors colors
    renderIfNotColliding '+' shipCoords space worldCorner
    restoreColors c)

renderNumber :: Number -> Space -> RenderState -> IO ()
renderNumber (Number (PosSpeed pos _) i) space r = do
  fg <- setRawForeground $ numberColor i
  renderIfNotColliding (intToDigit i) pos space r
  restoreForeground fg
