{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Hamazed.World.Draw
        ( drawUIAnimation
        , drawWorld
        ) where

import           Imj.Prelude

import           Data.Char( intToDigit )
import           Data.Maybe( isNothing, isJust )

import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Game.Hamazed.World.Space
import           Imj.Game.Hamazed.World.Types
import           Imj.Geo.Discrete
import           Imj.Graphics.UI.Animation
import           Imj.Physics.Discrete.Collision


{-# INLINABLE drawWorld #-}
drawWorld :: (Draw e, MonadReader e m, MonadIO m)
            => World
            -> Coords Pos
            -> m ()
drawWorld (World balls ships space _) s  = do
  -- draw numbers, including the ones that will be destroyed, if any
  mapM_ (\b -> drawNumber b space s) balls
  let drawShip (BattleShip shipId (PosSpeed shipCoords _) _ safeTime collisions) =
        when ((null collisions || isJust safeTime) && (InsideWorld == location shipCoords space)) $ do
          let colors =
                if isNothing safeTime
                  then
                    shipColors
                  else
                    shipColorsSafe
          drawChar (shipChar shipId) (sumCoords shipCoords s) colors
  mapM_ drawShip ships

shipChar :: ShipId -> Char
shipChar _ = '+' -- TODO change drawing char based on id

{-# INLINABLE drawNumber #-}
drawNumber :: (Draw e, MonadReader e m, MonadIO m)
             => Number
             -> Space
             -> Coords Pos
             -> m ()
drawNumber (Number (PosSpeed pos _) i) space b =
  when (location pos space == InsideWorld) $
    drawChar (intToDigit i) (sumCoords pos b) (numberColor i)
