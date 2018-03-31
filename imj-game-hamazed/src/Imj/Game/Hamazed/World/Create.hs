{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Hamazed.World.Create
        ( mkWorldEssence
        , mkMinimalWorldEssence
        , mkSpace
        , updateMovableItem
        ) where

import           Imj.Prelude
import           Prelude(length)

import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Data.List(sortOn)
import qualified Data.Map.Strict as Map(empty, fromDistinctAscList)
import qualified Data.Set as Set(size, toAscList, empty)
import           System.Random.MWC(GenIO, withSystemRandom, asGenIO)

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

mkWorldEssence :: WorldSpec -> IO Bool -> IO (MkSpaceResult WorldEssence, Maybe Statistics)
mkWorldEssence (WorldSpec s@(LevelSpec levelNum _) shipIds (WorldParameters shape wallDistribution)) continue =
-- withSystemRandom seeds a PRNG with data from the system's fast source of pseudo-random numbers.
-- The generator should be used from a single thread.
  withSystemRandom . asGenIO $ \gen -> go (max 1 $ fromIntegral nShips) [] Nothing gen
 where
  (LevelEssence _ _ numbers) = mkLevelEssence s
  size = worldSizeFromLevel levelNum shape
  nShips = Set.size shipIds

  go x y z gen =
    go' x y z
   where
    go' 0 errs stats = return (Impossible errs, stats)
    go' n errs prevStats = do
      (mkSpaceRes, stats) <- mkSpace size n wallDistribution continue gen
      let newStats = mergeMayStats prevStats stats
      case mkSpaceRes of
        Impossible err -> -- reduce the number of components asked for:
          go' (n-1) (err ++ errs) newStats
        NeedMoreTime -> return (NeedMoreTime, newStats)
        Success (BigWorld space topology) -> do -- convert BigWorld to WorldEssence
          let associations = map (\(a,b,c) -> Association a b c) $ zip3
                (Set.toAscList shipIds)
                (mkGroups nShips numbers) -- TODO shuffle numbers ?
                $ cycle $ getComponentIndices topology
          -- TODO make groups such that a single player cannot finish the level without the help of others.
          shipsAndNums <- mapM
            (\(Association shipId nums componentIdx) -> do
              -- TODO shuffle this, as it is ordered due to the use of a Set inside.
               positions <- randomCCCoords gen (1 + length nums) componentIdx topology NoOverlap
               (shipPS:numPS) <- mapM (mkRandomPosSpeed gen space) positions
               let collisions = Set.empty -- no collision because we passed 'NoOverlap' to randomCCCoords
                   ship = BattleShip shipId shipPS initialLaserAmmo Armored collisions componentIdx
                   ns = map (\(num,posSpeed) -> NumberEssence posSpeed num componentIdx) $ zip nums numPS
               return ((shipId,ship),ns)
            ) associations
          let (ships, balls) = unzip shipsAndNums
          return (Success $ WorldEssence
                    -- we sort the numbers so that their order in the map matches the order of their numeric values
                    (Map.fromDistinctAscList $ zip (map NumId [0..]) $ sortOn getNumber $ concat balls)
                    (Map.fromDistinctAscList ships)
                    space
                  , newStats)


mkMinimalWorldEssence :: WorldEssence
mkMinimalWorldEssence = WorldEssence Map.empty Map.empty mkZeroSpace

mkSpace :: (MonadIO m)
        => Size
        -- ^ The dimensions
        -> ComponentCount
        -- ^ Number of connex components
        -> WallDistribution
        -- ^ How the 'Wall's should be constructed
        -> IO Bool
        -- ^ Returns false when we should stop trying
        -> GenIO
        -> m (MkSpaceResult BigWorld, Maybe Statistics)
mkSpace s n params@(WallDistribution _ proba) continue gen
 | proba > 0 = liftIO $ mkRandomlyFilledSpace params s n continue gen
-- with 0 probability, we can't do anything else but return an empty space:
 | otherwise = return (Success $ mkEmptySpace s, Nothing)

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
