{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Game.Hamazed.World.Ship
        ( shipParticleSystems
        , countAmmo
        , updateShipsText
        ) where

import           Imj.Prelude

import           Data.Char( intToDigit )
import           Data.List( foldl' )
import           Data.Map.Strict(elems, traverseWithKey)

import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.State.Types
import           Imj.Game.Hamazed.World.Space.Types

import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.Infos
import           Imj.Game.Hamazed.Loop.Event.Priorities
import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Geo.Discrete
import           Imj.Geo.Continuous
import           Imj.Graphics.ParticleSystem
import           Imj.Graphics.UI.Animation
import           Imj.Graphics.UI.RectContainer


{- | If the ship is colliding and not in "safe time", and the event is a gamestep,
this function creates an animation where the ship and the colliding number explode.

The ship 'ParticleSystem' will have the initial speed of the number and vice-versa,
to mimic the rebound due to the collision. -}
shipParticleSystems :: (MonadState AppState m)
                    => Time Point System
                    -> m [Prioritized ParticleSystem]
shipParticleSystems k =
  getWorld >>= \w -> do
    envFuncs <- envFunctions $ WorldScope Air
    let sps _ (BattleShip _ _ _ Armored _) = return []
        sps _ (BattleShip _ _ _ Unarmored _) = return []
        sps _ (BattleShip _ _ _ Destroyed []) = return []
        sps shipId (BattleShip _ (PosSpeed shipCoords shipSpeed) _ Destroyed collisions) = getPlayer shipId >>= maybe
          (return [])
          (\(Player _ _ (PlayerColors _ cycles)) -> do
            -- when number and ship explode, they exchange speeds
            let collidingNumbersAvgSpeed =
                  foldl' sumCoords zeroCoords
                  $ map (\(Number (PosSpeed _ speed) _) -> speed) collisions
                numSpeed = scalarProd 0.4 $ speed2vec collidingNumbersAvgSpeed
                shipSpeed2 = scalarProd 0.4 $ speed2vec shipSpeed
                (Number _ n) = head collisions
                k' = systemTimePointToParticleSystemTimePoint k
            let color i =
                  cycleColors sumFrameParticleIndex $
                    if even i
                      then outer1 cycles
                      else wall2 cycles
            return
              $ map (Prioritized particleSystDefaultPriority)
              $ fragmentsFreeFallThenExplode numSpeed shipCoords color '|' (Speed 1) envFuncs k' ++
                fragmentsFreeFallThenExplode shipSpeed2 shipCoords color (intToDigit n) (Speed 1) envFuncs k')
    concat <$> traverseWithKey sps (getWorldShips w)

countAmmo :: [BattleShip] -> Int
countAmmo =
  foldl' (\s (BattleShip _ _ ammo status _) ->
            if shipIsAlive status
              then
                s + ammo
              else
                s) 0

{-# INLINABLE updateShipsText #-}
updateShipsText :: (MonadState AppState m)
                => m ()
updateShipsText =
  getGameState >>= \(GameState (World _ ships space _ _ _) _ shotNumbers (Level level _)
                               (UIAnimation (UIEvolutions j upDown _) p) (Screen _ center) mode names) -> do
    let newLeft =
          let frameSpace = mkRectContainerWithCenterAndInnerSize center $ getSize space
              (horizontalDist, verticalDist) = computeViewDistances mode
              (_, _, leftMiddle, _) = getSideCenters $ mkRectContainerAtDistance frameSpace horizontalDist verticalDist
              infos = mkLeftInfo Normal (elems ships) names shotNumbers level
          in mkTextAnimRightAligned leftMiddle leftMiddle infos 1 (fromSecs 1)
        newAnim = UIAnimation (UIEvolutions j upDown newLeft) p -- TODO use mkUIAnimation to have a smooth transition
    putAnimation newAnim
