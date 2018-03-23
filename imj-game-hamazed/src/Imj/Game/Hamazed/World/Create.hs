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
import qualified Data.Map.Strict as Map(empty, fromList)
import qualified Data.Set as Set(size, toList, empty)
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

mkWorldEssence :: WorldSpec -> IO Bool -> IO (Maybe WorldEssence, Maybe Statistics)
mkWorldEssence (WorldSpec s@(LevelSpec levelNum _) shipIds (WorldParameters shape wallDistribution)) continue =
-- withSystemRandom seeds a PRNG with data from the system's fast source of pseudo-random numbers.
-- The generator should be used from a single thread.
 withSystemRandom . asGenIO $ \gen -> do
  let (LevelEssence _ _ numbers) = mkLevelEssence s
      size = worldSizeFromLevel levelNum shape
      nShips = Set.size shipIds
  (maySpaceTopo, stats) <- mkSpace size (fromIntegral nShips) wallDistribution continue gen
  maybe
    (return (Nothing, stats))
    (\(space, topology) -> do
      let associations = map (\(a,b,c) -> Association a b c) $ zip3
            (Set.toList shipIds)
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
               ns = map (\(n,posSpeed) -> NumberEssence posSpeed n componentIdx) $ zip nums numPS
           return ((shipId, ship), ns)
        ) associations
      let (ships, balls) = unzip shipsAndNums
      return (Just $ WorldEssence
                -- we sort the numbers so that their order in the map matches the order of their numeric values
                (Map.fromList $ zip (map NumId [0..]) $ sortOn getNumber $ concat balls)
                (Map.fromList ships)
                (toListOfLists space)
              , stats))
    maySpaceTopo


mkMinimalWorldEssence :: WorldEssence
mkMinimalWorldEssence = WorldEssence Map.empty Map.empty (MaterialMatrix [[]])

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
        -> m (Maybe (Space, BigWorldTopology), Maybe Statistics)
mkSpace s n dist continue gen = case dist of
  None -> let (a,b) = mkEmptySpace s in return (Just (a,b),Nothing)
  Random params -> liftIO $ mkRandomlyFilledSpace params s n continue gen

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
