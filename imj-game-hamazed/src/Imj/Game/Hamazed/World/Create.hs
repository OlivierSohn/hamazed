{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Game.Hamazed.World.Create
        ( mkWorldEssence
        , mkMinimalWorldEssence
        , mkSpace
        , updateMovableItem
        , validateScreen
        ) where

import           Imj.Prelude
import           Prelude(length)

import           Control.Monad.IO.Class(MonadIO, liftIO)
import qualified Data.Map.Strict as Map(empty, fromList)
import qualified Data.Set as Set(size, toList)

import           Imj.Game.Hamazed.Level.Types
import           Imj.Game.Hamazed.World.Types
import           Imj.Game.Hamazed.World.Space.Types

import           Imj.Game.Hamazed.World.Size
import           Imj.Game.Hamazed.World.Space
import           Imj.Geo.Discrete
import           Imj.Physics.Discrete.Collision
import           Imj.Util

data Association = Association {
    _shipId :: {-# UNPACK #-} !ShipId
  , _numbers :: [Int]
  , _componentIdx :: {-# UNPACK #-} !ComponentIdx
} deriving(Show)

mkWorldEssence :: WorldSpec -> IO WorldEssence
mkWorldEssence (WorldSpec s@(LevelSpec levelNum _) shipIds (WorldParameters shape wallDistribution) wid) = do
  let (LevelEssence _ _ numbers) = mkLevelEssence s
      size = worldSizeFromLevel levelNum shape
      nShips = Set.size shipIds
  (space, topology) <- mkSpace size nShips wallDistribution
  let associations = map (\(a,b,c) -> Association a b c) $ zip3
        (Set.toList shipIds)
        (mkGroups nShips numbers) -- TODO shuffle numbers ?
        $ cycle $ getComponentIndices topology
  -- TODO make groups such that a single player cannot finish the level without the help of others.
  shipsAndNums <- mapM
    (\(Association shipId nums componentIdx) -> do
      -- TODO shuffle this, as it is ordered due to the use of a Set inside.
       positions <- randomCCCoords (1 + length nums) componentIdx topology NoOverlap
       (shipPosSpeed:numPosSpeeds) <- mapM (mkRandomPosSpeed space) positions
       let ship = BattleShip shipId shipPosSpeed initialLaserAmmo Armored [] -- no collision because we passed 'NoOverlap' to randomCCCoords
           ns = map (\(n,posSpeed) -> Number posSpeed n) $ zip nums numPosSpeeds
       return ((shipId, ship), ns)
    ) associations
  let (ships, balls) = unzip shipsAndNums

  --balls <- mapM (createRandomNumber space) numbers
  --posSpeeds <- liftIO $ createShipPosSpeeds nShips space (map (\(Number (PosSpeed pos _) _) -> pos) balls) []
  --let ships = fromList $ map
  --      (\(shipId,posSpeed@(PosSpeed pos _)) ->
  --        (shipId, BattleShip shipId posSpeed initialLaserAmmo Armored (getColliding pos balls)))
  --      $ zip clientIds posSpeeds
  return $ WorldEssence (concat balls) (Map.fromList ships) (toListOfLists space) wid

mkMinimalWorldEssence :: WorldEssence
mkMinimalWorldEssence = WorldEssence [] Map.empty (MaterialMatrix [[]]) Nothing

mkSpace :: (MonadIO m)
        => Size
        -- ^ The dimensions
        -> Int
        -- ^ Number of connex components
        -> WallDistribution
        -- ^ How the 'Wall's should be constructed
        -> m (Space, BigWorldTopology)
mkSpace s n = \case
  None -> return $ mkEmptySpace s
  Random params -> liftIO $ mkRandomlyFilledSpace params s n

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
