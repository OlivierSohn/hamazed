{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}

module Imj.Game.Hamazed.World.Create
        ( mkWorldEssence
        , mkMinimalWorldEssence
        , updateMovableItem
        ) where

import           Imj.Prelude
import           Prelude(length)
import qualified Prelude as Unsafe(last)

import           Control.Monad.IO.Class(liftIO)
import           Data.List(sortOn, concat)
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as Map(empty, fromDistinctAscList)
import qualified Data.Set as Set(size, toAscList, empty)
import           System.Random.MWC(GenIO)

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

mkMinimalWorldEssence :: WorldEssence
mkMinimalWorldEssence = WorldEssence Map.empty Map.empty mkZeroSpace


mkWorldEssence :: WorldSpec -> IO Bool -> NonEmpty GenIO -> IO (MkSpaceResult WorldEssence, Map Properties Statistics)
mkWorldEssence (WorldSpec s@(LevelSpec levelNum _) shipIds (WorldParameters shape wallDist@(WallDistribution _ proba))) continue gens@(gen:|_) =
  -- 4 is the max number of components in the file containing optimal strategies.
  go (min 4 $ max 1 $ fromIntegral nShips) [] Map.empty
 where
  (LevelEssence _ _ numbers) = mkLevelEssence s
  size = worldSizeFromLevel levelNum shape
  nShips = Set.size shipIds

  go 0 errs stats = return (Impossible errs, stats)
  go n !errs prevStats = do
    (mkSpaceRes, stats) <- mkSpace
    let !newStats = safeMerge mergeStats prevStats stats
    case mkSpaceRes of
      Impossible err -> -- reduce the number of components asked for:
        go (n-1) (err ++ errs) newStats
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
                 ship = BattleShip shipPS initialLaserAmmo Armored collisions componentIdx
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
   where
    mkSpace
     | proba > 0 =
        fmap (Map.fromDistinctAscList . maybeToList) <$>
          liftIO (mkRandomlyFilledSpace wallDist size n continue gens)
      -- with 0 probability, we can't do anything else but return an empty space:
     | otherwise = return (Success $ mkEmptySpace size, Map.empty)

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
  in case trajectory of
    [] -> error "logic"
    _:_ ->
      let newPos = maybe (Unsafe.last trajectory) snd $ firstCollision (`location` space) trajectory
      in PosSpeed newPos speed
