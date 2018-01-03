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
import           Imj.Geo.Discrete
import           Imj.Graphics.Animation.Design.Types
import           Imj.Graphics.Animation.Design.Draw
import           Imj.Graphics.UI.RectArea

-- | Renders the game to the screen, using "Imj.Graphics.Render.Delta" to avoid
-- <https://en.wikipedia.org/wiki/Screen_tearing screen tearing>.
{-# INLINABLE render #-}
render :: (Render e, MonadReader e m, MonadIO m)
       => GameState -> m ()
render (GameState _ world@(World _ _ space animations (InTerminal _ mode view@(RectArea from to)))
                  _ _ level wa) = do
  let getOffset (World _ (BattleShip (PosSpeed shipPos _) _ _ _) _ _ _) =
          let (Coords h w) = diffCoords to from
              screenCenter = Coords (quot h 2) (quot w 2)
          in case mode of
              CenterSpace -> zeroCoords
              CenterShip  -> diffCoords screenCenter shipPos
      offset = getOffset world
      curUpperLeft = translate from offset

  -- draw the walls outside the matrix:
  fill (materialChar Wall) outerWallsColors
  -- draw the matrix:
  drawSpace space curUpperLeft >>=
    (\worldCorner -> do
        drawAnimations worldCorner animations
        drawWorld world worldCorner)
  drawUIAnimation offset wa -- draw it after the world so that when it morphs
                            -- it goes over numbers and ship
  -- draw last so that the message is clearly visible:
  drawLevelMessage level (rectAreaCenter view)
  renderToScreen

{-# INLINABLE drawAnimations #-}
drawAnimations :: (Draw e, MonadReader e m, MonadIO m)
                 => Coords Pos
                 -> [Animation]
                 -> m ()
drawAnimations worldCorner animations = do
  let drawAnimation a = drawAnim a worldCorner
  -- animations are relative to the (moving) worldcorner.
  -- we should split animations in 2 : relative to the world, relative to the terminal.
  --
  mapM_ drawAnimation animations
