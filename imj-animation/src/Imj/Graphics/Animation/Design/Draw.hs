{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Animation.Design.Draw
    ( drawAnim
    ) where


import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import           Data.Either(partitionEithers)

import           Imj.Geo.Continuous
import           Imj.Geo.Discrete
import           Imj.Graphics.Animation.Design.Types
import           Imj.Graphics.Render
import           Imj.Iteration
import           Imj.Physics.Continuous.Types

{- | If an 'Particle' has no specific 'Char' to be drawn with,
it will be drawn with the 'Char' of the 'Animation'.

Hence, if neither 'Particle' nor 'Animation' contain a 'Char', this function
errors.-}
{-# INLINABLE drawAnim #-}
drawAnim :: (Draw e, MonadReader e m, MonadIO m)
           => Animation
           -> Coords Pos
           -- ^ Reference coordinates.
           -> m ()
drawAnim (Animation points _ (EnvFunctions interaction _) (UpdateSpec _ (Iteration _ frameForNextUpdate))) =
  draw' frameForNextUpdate points interaction

{-# INLINABLE draw' #-}
draw' :: (Draw e, MonadReader e m, MonadIO m)
        => Frame
        -> Particles
        -> (Coords Pos -> InteractionResult)
        -> Coords Pos
        -> m ()
draw' _ (Particles Nothing _ _) _ _   = return ()
draw' _ (Particles (Just []) _ _) _ _ = return ()
draw'
 parentFrame (Particles (Just branches) _ childFrame) interaction r = do
  let (children, aliveCoordinates) = partitionEithers branches
      selectDrawnCoordinates =
        filter (\(Particle canInteract (VecPosSpeed coords _) _ _) ->
                    case canInteract of
                      -- An alive particle may collide:
                      DontInteract -> interaction (vec2pos coords) == Stable
                      -- We make the assumption that every alive point is guaranteed to be collision-free.
                      -- Note that when the environment will be dynamic, it will be wrong:
                      Interact -> True)
      relFrame = parentFrame - childFrame
  mapM_ (\(Particle _ (VecPosSpeed vc _) char color) -> do
            let c = vec2pos vc
            drawChar char (sumCoords c r) color)
        $ selectDrawnCoordinates aliveCoordinates
  mapM_ (\child -> draw' relFrame child interaction r) children
