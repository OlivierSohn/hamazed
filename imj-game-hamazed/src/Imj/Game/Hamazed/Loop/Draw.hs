{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Game.Hamazed.Loop.Draw
      ( draw
      ) where

import           Imj.Prelude

import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.Level
import           Imj.Game.Hamazed.Loop.Event
import           Imj.Game.Hamazed.Loop.Event.Priorities
import           Imj.Game.Hamazed.State.Types
import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Game.Hamazed.World.Space
import           Imj.Game.Hamazed.World
import           Imj.Graphics.ParticleSystem.Design.Draw

-- | Draws the game content.
{-# INLINABLE draw #-}
draw :: (MonadState AppState m, Draw e, MonadReader e m, MonadIO m)
     => m ()
draw =
  getGame >>= \(Game _ mode
                     (GameState world@(World _ _ _ renderedSpace animations _) _ _ level wa (Screen _ screenCenter))
                     _ _ _ _) -> do
    let offset = getWorldOffset mode world
        worldCorner = getWorldCorner world screenCenter offset
    -- draw the walls outside the matrix:
    fill (materialChar Wall) outerWallsColors
    -- draw the matrix:
    drawSpace renderedSpace worldCorner
    mapM_ (\(Prioritized _ a) -> drawSystem a worldCorner) animations
    drawWorld world worldCorner
    drawUIAnimation offset wa -- draw it after the world so that when it morphs
                              -- it goes over numbers and ship
    -- draw last so that the message is clearly visible:
    drawLevelMessage level screenCenter
