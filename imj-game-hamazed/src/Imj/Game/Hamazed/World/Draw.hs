{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Game.Hamazed.World.Draw
        ( drawUIAnimation
        , drawWorld
        ) where

import           Imj.Prelude
import           Control.Monad.Reader.Class(MonadReader)

import           Data.Char(intToDigit)
import qualified Data.Map.Strict as Map(assocs)

import           Imj.Game.Class
import           Imj.Game.Modify
import           Imj.Game.Hamazed.World.Types
import           Imj.Graphics.Render
import           Imj.Space.Types

import           Imj.Game.Hamazed.Color
import           Imj.Graphics.Font
import           Imj.Graphics.UI.Animation
import           Imj.Physics.Discrete.Collision
import           Imj.Space

{-# INLINABLE drawWorld #-}
drawWorld :: (MonadState (AppState s) m, Draw e, MonadReader e m, MonadIO m)
          => World
          -> Coords Pos
          -> m ()
drawWorld (World balls ships space _ _) s  = do
  -- draw numbers, including the ones that will be destroyed, if any
  mapM_ (drawNumber space s) balls
  mapM_ drawShip $ Map.assocs ships
 where
  drawShip (i, BattleShip (PosSpeed shipCoords _) _ status _ _) = do
    let absPos = sumCoords shipCoords s
        inWorld = InsideWorld == location shipCoords space
        f bg =
          when inWorld $
            maybe
              shipColor
              (getPlayerColor . getPlayerColors)
              <$> getPlayer i >>= drawGlyph (gameGlyph '+') absPos . LayeredColor bg
    case status of
      Armored   -> f shipBgColorSafe
      Unarmored -> f shipBgColor
      Destroyed -> return ()

{-# INLINABLE drawNumber #-}
drawNumber :: (Draw e, MonadReader e m, MonadIO m)
           => Space
           -> Coords Pos
           -> Number
           -> m ()
drawNumber space b n@(Number (NumberEssence (PosSpeed pos _) i _) _ _) =
  when (location pos space == InsideWorld) $
    drawGlyph (gameGlyph $ intToDigit i) (sumCoords pos b) $ onBlack $ getCurrentColor n
