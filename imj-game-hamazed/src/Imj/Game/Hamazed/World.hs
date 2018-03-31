{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Game.Hamazed.World
    ( -- * Level
      {-| There are 12 levels in Hamazed, numbered from 1 to 12.
      -}
      Level(..)
      -- ** Level termination
      {-| [Target number]
      Each level has a different /target number/ which represents the sum of shot
      'Number's that should be reached to finish the 'Level'.

       A 'Level' is finished once the sum of shot 'Number's amounts to the /target number/. -}
    , checkTargetAndAmmo
    , checkAllComponentStatus
    , checkSums
    -- * World
    {- | A 'World' brings together:

    * game elements : 'Space', 'BattleShip' and 'Number's,
    * drawing elements: 'ParticleSystem's,
    * terminal-awareness : 'InTerminal'
    -}
    , World(..)
    -- ** World size
      {-|
      The 'World' size decreases with increasing 'Level' numbers.

      'worldSizeFromLevel' gives the 'Size' of the 'World' based on
      the 'Level' number and the 'WorldShape':
      -}
    , worldSizeFromLevel
    -- ** Update World
    -- | Every 'gameMotionPeriod' seconds, the positions of 'BattleShip' and 'Numbers'
    -- are updated according to their speeds:
    , gameMotionPeriod
    , moveWorld
    -- ** Draw World
    , drawWorld
    -- ** World utilities
    -- | 'laserEventAction' returns the effect a laser shot it has on the 'World'.
    , laserEventAction
    -- * Space
    {-| 'Space' describes the environment in which 'Number's and the 'BattleShip'
    live.

    It can be composed of 'Air', where 'BattleShip' and 'Number's are free to move, and of
    'Wall'.
    -}
    , Space
    , Material(..)
      -- ** Simple creation
    , mkEmptySpace
      -- ** Randomized creation
      {-| 'mkRandomlyFilledSpace' places 'Wall's at random and discards resulting
      'Space's which have more than one 'Air' connected component.

      This way, the 'BattleShip' is guaranteed to be able to reach any part of
      the 'Space', and 'Number's.

      Generating a big 'Space' with a very small block size can
      be computationnaly expensive because the probability to have a single 'Air'
      connected component drops very fast (probably at least exponentially)
      towards zero with increasing sizes. -}
    , mkRandomlyFilledSpace
      -- ** Collision detection
      {- | 'location' is the standard collision detection function that considers
      that being outside the world means being in collision.

      'scopedLocation' provides more options with the use of 'Scope' to
      defines the collision detection scopes.
       -}
    , location
    , scopedLocation
    , Scope(..)
      -- ** Draw
    , drawSpace
      -- * Movable items
      -- | A movable item's 'PosSpeed' is updated using 'updateMovableItem'
      -- at each 'MoveFlyingItems' event:
    , updateMovableItem
      -- ** BattleShip
    , BattleShip(..)
    -- ** Number
    -- | 'Number's can be shot by the 'BattleShip' to finish the 'Level'.
    --
    -- Number can collide with the 'BattleShip', hence triggering colorfull
    -- 'ParticleSystem' explosions.
    --
    -- 'Number's never change speed, except when they rebound on 'Wall's, of course.
    , Number(..)
    -- * UI
    {- | UI elements around the 'World' are:

    * a 'RectContainer' created by 'mkRectContainerWithTotalArea' to visually delimit the 'World'
    * 'ColorString' information, placed around the 'RectContainer':

        * Up: 'Level' target
        * Left: remaining ammunitions / shot 'Number's
        * Down: 'Level' number
    -}
    , mkRectContainerWithTotalArea
    -- ** Inter-level animations
    , module Imj.Graphics.UI.Animation
    -- * Secondary types
    , WallDistribution(..), WorldShape(..), ViewMode(..)
    , LevelOutcome(..)
    -- * Reexports
    , module Imj.Graphics.Render
    ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import qualified Data.Set as Set(empty, null, member, fromList, fromDescList, fromDistinctDescList, unions, union, size)
import qualified Data.Map.Strict as Map(elems, insert, lookup, lookupMin, map, empty, null, keysSet, foldl', alter
                                      , findWithDefault, mapAccumWithKey, fromListWith)
import           Data.List(elem, length)
import           Data.Maybe(isJust)
import           Data.Text(pack)

import qualified Imj.Data.Tree as Tree(toList)
import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.State.Types
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.Color.Types
import           Imj.Graphics.ParticleSystem.Design.Types

import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.Loop.Event.Priorities
import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Game.Hamazed.World.Create
import           Imj.Game.Hamazed.World.Draw
import           Imj.Game.Hamazed.World.Number
import           Imj.Game.Hamazed.World.Ship
import           Imj.Game.Hamazed.World.Size
import           Imj.Game.Hamazed.World.Space.Draw
import           Imj.Game.Hamazed.World.Space
import           Imj.GameItem.Weapon.Laser
import           Imj.Geo.Continuous
import           Imj.Graphics.Render
import           Imj.Graphics.ParticleSystem
import           Imj.Graphics.UI.Animation
import           Imj.Graphics.UI.RectContainer
import           Imj.Physics.Discrete.Collision
import           Imj.Sums

-- | Moves elements of game logic ('Number's, 'BattleShip').
--
-- Note that 'ParticleSystem's are not updated.
moveWorld :: MonadState AppState m
          => Map ShipId (Coords Vel)
          -> Set ShipId
          -> m ()
moveWorld accelerations shipsLosingArmor = getWorld >>= \(World balls ships space rs anims f) -> do
  let newBalls =
        Map.map (\n@(Number e@(NumberEssence ps _ _) colors _) ->
                            n{ getNumEssence = e{ getNumPosSpeed = updateMovableItem space ps }
                             , getNumColor = case colors of
                                 _:rest -> rest -- move forward in color animation
                                 [] -> [] })
        balls
      moveShip comps sid (BattleShip name (PosSpeed prevPos oldSpeed) ammo status _ i) =
        let collisions =
              if shipIsAlive status
                then
                  Map.keysSet $ getColliding pos newBalls
                else
                  Set.empty
            newComps = comps ++ [ i | not $ Set.null collisions]
            destroyedOr x =
              if Set.null collisions
                then x
                else Destroyed
            newStatus =
              case status of
                Destroyed -> Destroyed
                Armored ->
                  if sid `elem` shipsLosingArmor
                    then
                      destroyedOr Unarmored
                    else
                      Armored
                Unarmored -> destroyedOr Unarmored
            newSpeed =
              maybe oldSpeed (sumCoords oldSpeed) $ Map.lookup sid accelerations
            newPosSpeed@(PosSpeed pos _) = updateMovableItem space $ PosSpeed prevPos newSpeed
        in (newComps, BattleShip name newPosSpeed ammo newStatus collisions i)
      (changedComponents, newShips) = Map.mapAccumWithKey moveShip [] ships
  putWorld $ World newBalls newShips space rs anims f
  mapM_ checkComponentStatus changedComponents

-- | Computes the effect of a laser shot on the 'World'.
laserEventAction :: (MonadState AppState m)
                 => ShipId
                 -> Direction
                 -- ^ The direction of the laser shot
                 -> Time Point System
                 -> m (Map NumId Number, Bool)
                 -- ^ 'Number's destroyed + ammo changed
laserEventAction shipId dir t =
  getWorld >>= \(World balls ships space rs d e) -> do
    let ship@(BattleShip _ (PosSpeed shipCoords _) ammo status _ component) = findShip shipId ships
        (maybeLaserRayTheoretical, newAmmo) =
          if ammo > 0 && shipIsAlive status
            then
              (Just $ shootLaserWithOffset shipCoords dir Infinite (`location` space)
             , pred ammo)
            else
              (Nothing
             , ammo)
        ((remainingBalls, destroyedBalls), maybeLaserRay) =
           maybe
             ((balls,Map.empty), Nothing)
             (\r -> Just <$> computeActualLaserShot balls (getPos . getNumPosSpeed . getNumEssence)
                                                    r DestroyFirstObstacle)
               maybeLaserRayTheoretical
        newShip = ship { getAmmo = newAmmo }
        newShips = Map.insert shipId newShip ships
    putWorld $ World remainingBalls newShips space rs d e

    let tps = systemTimePointToParticleSystemTimePoint t
    outerSpaceParticleSystems_ <-
      if Map.null destroyedBalls
        then
          maybe (return []) (outerSpaceParticleSystems tps shipId) maybeLaserRay
        else
          return []
    newSystems <- destroyedNumbersParticleSystems tps shipId dir destroyedBalls
    laserSystems <- maybe (return []) (laserParticleSystems tps shipId) maybeLaserRay
    addParticleSystems $ concat [newSystems, laserSystems, outerSpaceParticleSystems_]

    when (countLiveAmmo newShip == 0) $ checkComponentStatus component
    return (destroyedBalls, isJust maybeLaserRay)

checkComponentStatus :: (MonadState AppState m)
                     => ComponentIdx
                     -> m ()
checkComponentStatus i = countComponentAmmo i >>= \case
    0 ->
      getWorld >>= \w ->
        putWorld $ w {
          getWorldNumbers =
            Map.map
              (\n -> if i == getNumberCC (getNumEssence n)
                then
                  makeUnreachable n
                else
                  n
              ) $ getWorldNumbers w }
    _ ->
      return ()

checkAllComponentStatus :: (MonadState AppState m)
                        => m ()
checkAllComponentStatus = countComponentsAmmo >>= \ammos ->
  getWorld >>= \w -> do
    let nums = Map.map (\n -> case Map.findWithDefault 0 (getNumberCC $ getNumEssence n) ammos of
          0 -> makeUnreachable n
          _ -> n) $ getWorldNumbers w
    putWorld $ w { getWorldNumbers = nums }

countComponentAmmo :: (MonadState AppState m)
                     => ComponentIdx
                     -> m Int
countComponentAmmo i =
  Map.foldl'
    (\m ship@(BattleShip _ _ _ _ _ idx) ->
        if idx == i
          then
            m + countLiveAmmo ship
          else
            m)
    0 . getWorldShips <$> getWorld


countComponentsAmmo :: (MonadState AppState m)
                    => m (Map ComponentIdx Int)
countComponentsAmmo =
  Map.foldl'
    (\m ship@(BattleShip _ _ _ _ _ idx) ->
      let ammo = countLiveAmmo ship
          f Nothing = Just ammo
          f (Just x) = Just $ ammo + x
      in Map.alter f idx m)
    Map.empty . getWorldShips <$> getWorld

-- | Discard sums that don't match the live ammo per connex components.
-- If a reachable number is in no sum, draw it in red.
checkSums :: (MonadState AppState m)
          => m ()
checkSums = getGameState >>= \(GameState w@(World remainingNumbers _ _ _ _ _) _ shotNumbers
                                         (Level (LevelEssence _ (LevelTarget totalQty constraint) _) _) _ _ _ _ _) ->
  case constraint of
    CanOvershoot -> return ()
    CannotOvershoot -> do
      let shotQty = applyOperations $ reverse shotNumbers
          remainingQty = totalQty - shotQty
          remainingNums = map (getNumber . getNumEssence) $ Map.elems remainingNumbers

          asSet = Set.fromList remainingNums
          distinct = Set.size asSet == length remainingNums
          -- list of descending lists.
          allLists = bool
            (mkSumsN remainingNums remainingQty)
            -- there is a 4x performance benefit in using 'mkSumsStrict' when numbers are unique.
            (Tree.toList $ mkSumsStrict asSet remainingQty)
            distinct
          -- TODO group numbers by cc, then check if cc has enough ammo.
          -- (take into account that in case of multiple identical number,
          -- the same numbers could be in multiple components)
          possibleLists = allLists

          -- Set.unions has a linear complexity, overall we have a linear complexity here.
          possibleSets = bool
            (map Set.fromDescList)
            (map Set.fromDistinctDescList)
            distinct possibleLists

          okTargets = Set.unions possibleSets

          -- preferred numbers are on the preferred path (least length)
          mapLengths = Map.fromListWith Set.union (map (\l -> (Set.size l,l)) possibleSets)
          preferredSet = maybe Set.empty snd $ Map.lookupMin mapLengths

          newNumbers =
            Map.map
              (\n -> let num = getNumber $ getNumEssence n
                in if Set.member num okTargets
                  then
                    if Set.member num preferredSet
                      then
                        makePreferred n
                      else
                        makeReachable n
                  else
                    makeDangerous n)
            remainingNumbers
      putWorld $ w { getWorldNumbers = newNumbers }


outerSpaceParticleSystems :: (MonadState AppState m)
                          => Time Point ParticleSyst
                          -> ShipId
                          -> LaserRay Actual
                          -> m [Prioritized ParticleSystem]
outerSpaceParticleSystems t shipId ray@(LaserRay dir _ _) = getPlayer shipId >>= maybe
  (return [])
  (\(Player _ _ (PlayerColors _ cycles)) -> do
    world <- getWorld
    let space = getWorldSpace world
        laserTarget = afterEnd ray
        glyph = materialGlyph Wall
    case location laserTarget space of
          InsideWorld -> return []
          OutsideWorld ->
            if distanceToSpace laserTarget space > 0
              then do
                let color _fragment _level _frame =
                      if 0 == _fragment `mod` 2
                        then
                          cycleColors sumFrameParticleIndex (outer1 cycles) $ quot _frame 4
                        else
                          cycleColors sumFrameParticleIndex (outer2 cycles) $ quot _frame 4
                    pos = translateInDir dir laserTarget
                    (speedAttenuation, nRebounds) = (0.3, 3)
                mode <- getViewMode
                screen <- getCurScreen
                case scopedLocation world mode screen NegativeWorldContainer pos of
                    InsideWorld -> outerSpaceParticleSystems' NegativeWorldContainer pos
                                    dir speedAttenuation nRebounds color glyph t
                    OutsideWorld -> return []
              else do
                let color _fragment _level _frame =
                      if 0 == _fragment `mod` 3
                        then
                          cycleColors sumFrameParticleIndex (wall1 cycles) $ quot _frame 4
                        else
                          cycleColors sumFrameParticleIndex (wall2 cycles) $ quot _frame 4
                    (speedAttenuation, nRebounds) = (0.4, 5)
                outerSpaceParticleSystems' (WorldScope Wall) laserTarget
                     dir speedAttenuation nRebounds color glyph t)

outerSpaceParticleSystems' :: (MonadState AppState m)
                           => Scope
                           -> Coords Pos
                           -> Direction
                           -> Float
                           -> Int
                           -> (Int -> Int -> Colorization)
                           -> Glyph
                           -> Time Point ParticleSyst
                           -> m [Prioritized ParticleSystem]
outerSpaceParticleSystems' scope afterLaserEndPoint dir speedAttenuation nRebounds colorFuncs glyph t = do
  envFuncs <- envFunctions scope
  let speed = scalarProd 0.8 $ speed2vec $ coordsForDirection dir
  return
    $ map (Prioritized particleSystDefaultPriority)
    $ fragmentsFreeFallWithReboundsThenExplode
      speed afterLaserEndPoint speedAttenuation nRebounds colorFuncs glyph
      (Speed 1) envFuncs t

laserParticleSystems :: (MonadState AppState m)
                     => Time Point ParticleSyst
                     -> ShipId
                     -> LaserRay Actual
                     -> m [Prioritized ParticleSystem]
laserParticleSystems t shipId ray = getPlayer shipId >>= maybe
  (return [])
  (\(Player _ _ (PlayerColors _ cycles)) ->
    return $ catMaybes
      [Prioritized particleSystLaserPriority <$> laserShot ray (cycleColors onlyFrame $ laser cycles) t])


checkTargetAndAmmo :: Int
                   -- ^ Remaining ammo
                   -> Int
                   -- ^ The current sum of all shot 'Numbers'
                   -> LevelTarget
                   -- ^ The 'Level' 's target number.
                   -> Maybe LevelOutcome
checkTargetAndAmmo ammo currentNumber (LevelTarget goal constraint) =
    checkSum <|> checkAmmo <|> Nothing
  where
    checkSum = case compare currentNumber goal of
      EQ -> Just Won
      LT -> Nothing
      GT -> case constraint of
        CanOvershoot -> Nothing
        CannotOvershoot -> Just $ Lost $ pack $ show currentNumber ++ " is bigger than " ++ show goal
    checkAmmo
      | ammo <= 0 = Just $ Lost $ pack "no ammo left"
      | otherwise = Nothing
