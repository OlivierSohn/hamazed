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
import           Imj.Game.Hamazed.State
import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Game.Hamazed.World.Space
import           Imj.Game.Hamazed.World
import           Imj.Graphics.ParticleSystem.Design.Draw

-- | Draws the game content.
{-# INLINABLE draw #-}
draw :: (MonadState AppState m, Draw e, MonadReader e m, MonadIO m)
     => GameState
     -> m ()
draw (GameState _ world@(World _ _ space animations) _ _ level wa (Screen _ screenCenter)) = do
  mode <- getMode
  let offset = getWorldOffset mode world
      worldCorner = getWorldCorner world screenCenter offset
  -- draw the walls outside the matrix:
  fill (materialChar Wall) outerWallsColors
  -- draw the matrix:
  drawSpace space worldCorner
  mapM_ (`drawSystem` worldCorner) animations
  drawWorld world worldCorner
  drawUIAnimation offset wa -- draw it after the world so that when it morphs
                            -- it goes over numbers and ship
  -- draw last so that the message is clearly visible:
  drawLevelMessage level screenCenter
