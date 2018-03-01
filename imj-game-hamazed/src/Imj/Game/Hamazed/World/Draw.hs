{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Game.Hamazed.World.Draw
        ( drawUIAnimation
        , drawWorld
        ) where

import           Imj.Prelude

import           Data.Char(intToDigit)
import           Data.Map.Strict(assocs)

import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.State.Types
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Game.Hamazed.World.Space
import           Imj.Game.Hamazed.World.Types

import           Imj.Game.Hamazed.Color
import           Imj.Geo.Discrete
import           Imj.Graphics.UI.Animation
import           Imj.Graphics.Font
import           Imj.Physics.Discrete.Collision


{-# INLINABLE drawWorld #-}
drawWorld :: (MonadState AppState m, Draw e, MonadReader e m, MonadIO m)
          => World
          -> Coords Pos
          -> m ()
drawWorld (World balls ships space _ _ _) s  = do
  -- draw numbers, including the ones that will be destroyed, if any
  mapM_ (\b -> drawNumber b space s) balls
  let drawShip (i, BattleShip _ (PosSpeed shipCoords _) _ status _) = do
        let absPos = sumCoords shipCoords s
            inWorld = InsideWorld == location shipCoords space
            go bg = when inWorld $ maybe shipColor (getPlayerColor . getPlayerColors) <$> getPlayer i >>=
              drawGlyph (gameGlyph '+') absPos . LayeredColor bg
        case status of
          Armored   -> go shipBgColorSafe
          Unarmored -> go shipBgColor
          Destroyed -> return ()
  mapM_ drawShip $ assocs ships

{-# INLINABLE drawNumber #-}
drawNumber :: (Draw e, MonadReader e m, MonadIO m)
           => Number
           -> Space
           -> Coords Pos
           -> m ()
drawNumber (Number (PosSpeed pos _) i) space b =
  when (location pos space == InsideWorld) $
    drawGlyph (gameGlyph $ intToDigit i) (sumCoords pos b) (numberColor i)
