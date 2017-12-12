{-# LANGUAGE NoImplicitPrelude #-}

module Game.World
    ( accelerateShip
    , mkWorld
    , moveWorld
    , nextWorld
    , withLaserAction
    , renderWorld
    , renderWorldAnimation
    , earliestAnimationDeadline
    -- | reexports
    , Number(..)
    , module Game.World.Types
    ) where

import           Imajuscule.Prelude

import           Data.Char( intToDigit )
import           Data.Maybe( isNothing )
import           Data.Time( addUTCTime
                          , getCurrentTime
                          , UTCTime )

import           Animation.Util( earliestDeadline )

import           Collision

import           Geo.Discrete.Bresenham
import           Geo.Discrete

import           Game.Color
import           Game.Event
import           Game.World.Evolution
import           Game.World.Laser
import           Game.World.Number
import           Game.World.Ship
import           Game.World.Space
import           Game.World.Types

import           Timing


accelerateShip :: Direction -> BattleShip -> BattleShip
accelerateShip dir (BattleShip (PosSpeed pos speed) ba bb bc) =
  let newSpeed = sumCoords speed $ coordsForDirection dir
  in BattleShip (PosSpeed pos newSpeed) ba bb bc

nextWorld :: World -> [Number] -> Int -> [BoundedAnimation] -> World
nextWorld (World _ changePos (BattleShip posspeed _ safeTime collisions) size _ e) balls ammo b =
  World balls changePos (BattleShip posspeed ammo safeTime collisions) size b e

-- move the world elements (numbers, ship), but do NOT advance the animations
moveWorld :: UTCTime -> World -> World
moveWorld curTime (World balls changePos (BattleShip shipPosSpeed ammo safeTime _) size anims e) =
  let newSafeTime = case safeTime of
        (Just t) -> if curTime > t then Nothing else safeTime
        _        -> Nothing
      newBalls = map (\(Number ps n) -> Number (changePos size ps) n) balls
      newPosSpeed@(PosSpeed pos _) = changePos size shipPosSpeed
      collisions = getColliding pos newBalls
      newShip = BattleShip newPosSpeed ammo newSafeTime collisions
  in World newBalls changePos newShip size anims e

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
earliestAnimationDeadline (World _ _ _ _ animations _) =
  earliestDeadline $ map (\(BoundedAnimation a _) -> a) animations

-- TODO use Number Live Number Dead
withLaserAction :: Event ->  World -> ([Number], [Number], Maybe (LaserRay Actual), Int)
withLaserAction
  event
  (World balls _ (BattleShip (PosSpeed shipCoords _) ammo safeTime collisions)
        space _ _)
 =
  let (maybeLaserRayTheoretical, newAmmo) =
       if ammo > 0 then case event of
         (Action Laser dir) ->
           (LaserRay dir <$> shootLaserFromShip shipCoords dir Infinite (`location` space), pred ammo)
         _ ->
           (Nothing, ammo)
       else
         (Nothing, ammo)

      ((remainingBalls', destroyedBalls), maybeLaserRay) =
         maybe
           ((balls,[]), Nothing)
           (survivingNumbers balls RayDestroysFirst)
             maybeLaserRayTheoretical

      remainingBalls = case event of
         Timeout GameStep _ ->
           if isNothing safeTime
             then
               filter (`notElem` collisions) remainingBalls'
             else
               remainingBalls'
         _ -> remainingBalls'
  in (remainingBalls, destroyedBalls, maybeLaserRay, newAmmo)

--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

mkWorld :: EmbeddedWorld -> WorldSize -> WallType -> [Int] -> Int -> IO World
mkWorld e s walltype nums ammo = do
  (Space mat sz render) <- case walltype of
    None          -> return $ mkEmptySpace s
    Deterministic -> return $ mkDeterministicallyFilledSpace s
    Random rParams    -> mkRandomlyFilledSpace rParams s
  t <- getCurrentTime
  let space = Space mat sz render
  balls <- mapM (createRandomNumber space) nums
  ship@(PosSpeed pos _) <- createShipPos space balls
  return $ World balls ballMotion (BattleShip ship ammo (Just $ addUTCTime 5 t) (getColliding pos balls)) space [] e

createRandomNumber :: Space -> Int -> IO Number
createRandomNumber space i = do
  ps <- createRandomPosSpeed space
  return $ Number ps i

renderWorld :: World -> IO ()
renderWorld (World balls _ (BattleShip (PosSpeed shipCoords _) _ safeTime collisions) space _ (EmbeddedWorld _ upperLeft buf))  = do
  -- render numbers, including the ones that will be destroyed, if any
  let s = translateInDir Down $ translateInDir RIGHT upperLeft
  mapM_ (\b -> renderNumber b space s buf) balls
  when (null collisions) (do
    let colors =
          if isNothing safeTime
            then
              shipColors
            else
              shipColorsSafe
    _ <- renderIfNotColliding '+' shipCoords space colors s buf -- TODO render if safetime or not colliding
    return ())

renderNumber :: Number -> Space -> Coords -> IORef Buffers -> IO (IORef Buffers)
renderNumber (Number (PosSpeed pos _) i) space r b = do
  let color = numberColor i
  renderIfNotColliding (intToDigit i) pos space color r b

renderWorldAnimation :: WorldAnimation
                     -> IORef Buffers
                     -> IO ()
renderWorldAnimation (WorldAnimation evolutions _ (Iteration (_, frame))) =
  renderEvolutions evolutions frame
