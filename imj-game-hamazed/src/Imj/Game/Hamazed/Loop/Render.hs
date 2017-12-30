{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Game.Hamazed.Loop.Render
      ( render
      ) where

import           Imj.Prelude

import           Imj.Game.Hamazed.Level
import           Imj.Game.Hamazed.Loop.Event
import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.World
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Graphics.Animation.Design.Types
import           Imj.Graphics.Animation.Design.Render
import           Imj.Graphics.UI.RectContainer


-- | Renders the game to the screen, using "Imj.Graphics.Render.Delta" to avoid
-- <https://en.wikipedia.org/wiki/Screen_tearing screen tearing>.
{-# INLINABLE render #-}
render :: (Render e, MonadReader e m, MonadIO m)
       => GameState -> m ()
render (GameState _ world@(World _ _ space animations (InTerminal _ curUpperLeft))
                  _ _ level wa) =
  renderSpace space curUpperLeft >>=
    (\worldCorner -> do
        renderAnimations worldCorner animations
        -- TODO merge 2 functions below (and no need to pass worldCorner)
        renderWorld world
        let (_,_,_,rightMiddle) = getSideCentersAtDistance (mkWorldContainer world) 3 2
        renderLevelMessage level rightMiddle
        renderUIAnimation wa -- render it last so that when it animates
                             -- to reduce, it goes over numbers and ship
        ) >> renderToScreen

{-# INLINABLE renderAnimations #-}
renderAnimations :: (Draw e, MonadReader e m, MonadIO m)
                 => Coords Pos
                 -> [Animation]
                 -> m ()
renderAnimations worldCorner animations = do
  let renderAnimation a = renderAnim a worldCorner
  mapM_ renderAnimation animations
