{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Game.Hamazed.World.Ship
        ( shipParticleSystems
        , countAmmo
        , countLiveAmmo
        , updateShipsText
        ) where

import           Imj.Prelude

import           Data.Char( intToDigit )
import           Data.List( foldl' )
import qualified Data.Map.Strict as Map(elems, traverseWithKey, restrictKeys, foldl', lookupMin)
import qualified Data.Set as Set(null)

import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.State.Types
import           Imj.Game.Hamazed.World.Space.Types

import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.Infos
import           Imj.Game.Hamazed.Loop.Event.Priorities
import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Geo.Continuous
import           Imj.Graphics.Font
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
    let sps _ (BattleShip _ _ _ Armored   _  _) = return []
        sps _ (BattleShip _ _ _ Unarmored _  _) = return []
        sps shipId (BattleShip _ (PosSpeed shipCoords shipSpeed) _ Destroyed collisions _) =
          if Set.null collisions
            then
              return []
            else getPlayer shipId >>= maybe
              (return [])
              (\(Player _ _ (PlayerColors _ cycles)) -> do
                -- when number and ship explode, they exchange speeds
                let collidingNumbersAvgSpeed =
                      Map.foldl'
                        (\s -> sumCoords s . getSpeed . getNumPosSpeed . getNumEssence)
                        zeroCoords
                        $ Map.restrictKeys (getWorldNumbers w) collisions
                    numSpeed = scalarProd 0.4 $ speed2vec collidingNumbersAvgSpeed
                    shipSpeed2 = scalarProd 0.4 $ speed2vec shipSpeed
                    k' = systemTimePointToParticleSystemTimePoint k
                    n = maybe '?' (intToDigit . getNumber . getNumEssence)
                      $ fmap snd $ Map.lookupMin $ Map.restrictKeys (getWorldNumbers w) collisions
                let color i =
                      cycleColors sumFrameParticleIndex $
                        if even i
                          then outer1 cycles
                          else wall2 cycles
                return
                  $ map (Prioritized particleSystDefaultPriority)
                  $ fragmentsFreeFallThenExplode numSpeed shipCoords color (gameGlyph '|') (Speed 1) envFuncs k' ++
                    fragmentsFreeFallThenExplode shipSpeed2 shipCoords color (gameGlyph n) (Speed 1) envFuncs k')
    concat <$> Map.traverseWithKey sps (getWorldShips w)

countAmmo :: [BattleShip] -> Int
countAmmo =
  foldl' (\s ship -> s + countLiveAmmo ship) 0

countLiveAmmo :: BattleShip -> Int
countLiveAmmo (BattleShip _ _ ammo status _ _) =
  if shipIsAlive status
    then
      ammo
    else
      0

{-# INLINABLE updateShipsText #-}
updateShipsText :: (MonadState AppState m)
                => m ()
updateShipsText =
  getGameState >>= \(GameState (World _ ships space _ _ _) _ shotNumbers (Level level _)
                               (UIAnimation (UIEvolutions j upDown _) p) _ (Screen _ center) mode names) -> do
    let newLeft =
          let frameSpace = mkRectContainerWithCenterAndInnerSize center $ getSize space
              (horizontalDist, verticalDist) = computeViewDistances mode
              (_, _, leftMiddle, _) = getSideCenters $ mkRectContainerAtDistance frameSpace horizontalDist verticalDist
              infos = mkLeftInfo Normal (Map.elems ships) names shotNumbers level
          in mkTextAnimRightAligned leftMiddle leftMiddle infos 1 (fromSecs 1)
        newAnim = UIAnimation (UIEvolutions j upDown newLeft) p -- TODO use mkUIAnimation to have a smooth transition
    putAnimation newAnim
