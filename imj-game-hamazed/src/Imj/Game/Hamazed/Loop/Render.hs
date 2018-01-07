{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Hamazed.Loop.Render
      ( render
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

-- | Renders the game to the screen, using "Imj.Graphics.Render.Delta" to avoid
-- <https://en.wikipedia.org/wiki/Screen_tearing screen tearing>.
{-# INLINABLE render #-}
render :: (Render e, MonadReader e m, MonadIO m)
       => GameState -> m ()
render (GameState _ world@(World _ _ space animations (InTerminal _ _ view))
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
  renderToScreen
