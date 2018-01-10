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
import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Game.Hamazed.World.Space
import           Imj.Game.Hamazed.World
import           Imj.Graphics.ParticleSystem.Design.Draw
import           Imj.Graphics.UI.RectArea

-- | Draws the game content.
{-# INLINABLE draw #-}
draw :: (Draw e, MonadReader e m, MonadIO m)
     => GameState -> m ()
draw (GameState _ world@(World _ _ space animations (InTerminal _ _ view))
                  _ _ level wa) = do
  let offset = getWorldOffset world
      worldCorner = getWorldCorner world offset
  -- draw the walls outside the matrix:
  fill (materialChar Wall) outerWallsColors
  -- draw the matrix:
  drawSpace space worldCorner
  mapM_ (`drawSystem` worldCorner) animations
  drawWorld world worldCorner
  drawUIAnimation offset wa -- draw it after the world so that when it morphs
                            -- it goes over numbers and ship
  -- draw last so that the message is clearly visible:
  drawLevelMessage level (rectAreaCenter view)
