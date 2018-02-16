{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Hamazed.World.Draw
        ( drawUIAnimation
        , drawWorld
        ) where

import           Imj.Prelude

import           Data.Char(intToDigit)
import           Data.Map.Strict(assocs)

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
drawWorld (World balls ships space _ _ _) s  = do
  -- draw numbers, including the ones that will be destroyed, if any
  mapM_ (\b -> drawNumber b space s) balls
  let drawShip (shipId, (BattleShip _ (PosSpeed shipCoords _) _ status _)) = do
        let char = shipChar shipId
            absPos = sumCoords shipCoords s
            inWorld = InsideWorld == location shipCoords space
        case status of
          Armored   -> when inWorld $ drawChar char absPos shipColorsSafe
          Unarmored -> when inWorld $ drawChar char absPos shipColors
          Destroyed -> return ()
  mapM_ drawShip $ assocs $ ships

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
