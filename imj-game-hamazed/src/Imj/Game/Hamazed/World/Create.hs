{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Game.Hamazed.World.Create
        ( mkWorldEssence
        , mkMinimalWorldEssence
        , mkSpace
        , updateMovableItem
        , validateScreen
        , createShipPos
        ) where

import           Imj.Prelude
import           Prelude(length)

import           Control.Monad.IO.Class(MonadIO, liftIO)

import           Imj.Game.Hamazed.Level.Types
import           Imj.Game.Hamazed.World.Types

import           Imj.Game.Hamazed.World.Size
import           Imj.Game.Hamazed.World.Space
import           Imj.Geo.Discrete
import           Imj.Physics.Discrete.Collision


mkWorldEssence :: WorldSpec -> IO WorldEssence
mkWorldEssence (WorldSpec levelNum shipIds (WorldParameters shape wallDistrib) wid) = do
  let (LevelSpec _ _ numbers) = mkLevelSpec levelNum
      size = worldSizeFromLevel levelNum shape
      nShips = length shipIds
  space <- mkSpace size wallDistrib
  balls <- mapM (createRandomNumber space) numbers
  posSpeeds <- liftIO $ createShipPosSpeeds nShips space (map (\(Number (PosSpeed pos _) _) -> pos) balls) []
  let ships = map
        (\(sid,posSpeed@(PosSpeed pos _)) -> BattleShip sid posSpeed initialLaserAmmo True (getColliding pos balls))
        $ zip shipIds posSpeeds
  return $ WorldEssence balls ships (toListOfLists space) wid

mkMinimalWorldEssence :: WorldEssence
mkMinimalWorldEssence = WorldEssence [] [] [[]] Nothing

initialLaserAmmo :: Int
initialLaserAmmo = 10

-- | Ships positions will not be colliding with numbers and with each other.
createShipPosSpeeds :: Int -> Space -> [Coords Pos] -> [PosSpeed] -> IO [PosSpeed]
createShipPosSpeeds 0 _ _ res = return res
createShipPosSpeeds n space obstacles cur =
  createShipPos space obstacles >>= \posSpeed@(PosSpeed pos _) ->
    createShipPosSpeeds (pred n) space (pos:obstacles) (posSpeed:cur)

mkSpace :: (MonadIO m)
        => Size
        -- ^ The dimensions
        -> WallDistribution
        -- ^ How the 'Wall's should be constructed
        -> m Space
mkSpace s = \case
  None          -> return $ mkEmptySpace s
  Deterministic -> return $ mkDeterministicallyFilledSpace s
  Random rParams    -> liftIO $ mkRandomlyFilledSpace rParams s

-- | Updates 'PosSpeed' of a movable item, according to 'Space'.
updateMovableItem :: Space
                  -- ^ The surrounding 'Space' will be taken into account for collisions.
                  -> PosSpeed
                  -- ^ The current position and speed of the moving item.
                  -> PosSpeed
                  -- ^ The updated position and speed.
updateMovableItem space ps@(PosSpeed pos _) =
  let (newPs@(PosSpeed newPos _), collision) =
        mirrorSpeedAndMoveToPrecollisionIfNeeded (`location` space) ps
  in  case collision of
        PreCollision ->
          if pos /= newPos
            then
              newPs
            else
              -- Precollision position is the same as the previous position, we try to move
              doBallMotionUntilCollision space newPs
        NoCollision  -> doBallMotion newPs

-- if we ever change this, we should chek other places where we use sumPosSpeed
-- to use this function instead
doBallMotion :: PosSpeed -> PosSpeed
doBallMotion (PosSpeed pos speed) =
  PosSpeed (sumPosSpeed pos speed) speed

-- | Changes the position until a collision is found.
--   Doesn't change the speed
doBallMotionUntilCollision :: Space -> PosSpeed -> PosSpeed
doBallMotionUntilCollision space (PosSpeed pos speed) =
  let trajectory = bresenham $ mkSegment pos $ sumPosSpeed pos speed
      newPos = maybe (last trajectory) snd $ firstCollision (`location` space) trajectory
  in PosSpeed newPos speed


createRandomNumber :: (MonadIO m)
                   => Space
                   -> Int
                   -> m Number
createRandomNumber space i = do
  ps <- liftIO $ createRandomNonCollidingPosSpeed space
  return $ Number ps i


createShipPos :: Space -> [Coords Pos] -> IO PosSpeed
createShipPos space numPositions = do
  candidate@(PosSpeed pos _) <- createRandomNonCollidingPosSpeed space
  if pos `notElem` numPositions
    then
      return candidate
    else
      createShipPos space numPositions

validateScreen :: Screen -> IO ()
validateScreen (Screen sz _) =
  case sz of
    Nothing -> return ()
    (Just winSize@(Size h w)) -> do
      let (Size rs cs) = maxWorldSize
          heightMargin = 2 * 1 {-outer walls-}
          widthMargin = 2 * (1 {-outer walls-} + 4 {-brackets, spaces-} + (9 + 6 * 2) {-display all numbers-})
          minSize@(Size minh minw) =
            Size (fromIntegral rs + heightMargin)
                 (fromIntegral cs + widthMargin)
      when (h < minh || w < minw) $
        error $ "\nMinimum discrete size : " ++ show minSize
            ++ ".\nCurrent discrete size : " ++ show winSize
            ++ ".\nThe current discrete size doesn't match the minimum size,"
            ++  "\nplease adjust your terminal or window size and restart the executable"
            ++ ".\n"
      return ()
