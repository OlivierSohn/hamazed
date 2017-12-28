{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Hamazed.World.Render
        ( renderUIAnimation
        , renderWorld
        ) where

import           Imj.Prelude

import           Data.Char( intToDigit )
import           Data.Maybe( isNothing, isJust )

import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Game.Hamazed.World.Space
import           Imj.Game.Hamazed.World.Types
import           Imj.Geo.Discrete
import           Imj.Physics.Discrete.Collision
import           Imj.Graphics.UI.Animation


{-# INLINABLE renderWorld #-}
renderWorld :: (Draw e, MonadReader e m, MonadIO m)
            => World
            -> m ()
renderWorld
  (World balls (BattleShip (PosSpeed shipCoords _) _ safeTime collisions)
         space _ (InTerminal _ upperLeft))  = do
  -- render numbers, including the ones that will be destroyed, if any
  let s = translateInDir Down $ translateInDir RIGHT upperLeft
  mapM_ (\b -> renderNumber b space s) balls
  when ((null collisions || isJust safeTime) && (InsideWorld == location shipCoords space)) $ do
    let colors =
          if isNothing safeTime
            then
              shipColors
            else
              shipColorsSafe
    drawChar '+' (sumCoords shipCoords s) colors


{-# INLINABLE renderNumber #-}
renderNumber :: (Draw e, MonadReader e m, MonadIO m)
             => Number
             -> Space
             -> Coords Pos
             -> m ()
renderNumber (Number (PosSpeed pos _) i) space b =
  when (location pos space == InsideWorld) $
    drawChar (intToDigit i) (sumCoords pos b) (numberColor i)
