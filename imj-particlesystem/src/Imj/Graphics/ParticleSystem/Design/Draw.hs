{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.ParticleSystem.Design.Draw
    ( drawSystem
    ) where


import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)
import           Data.Either(partitionEithers)

import           Imj.Geo.Continuous
import           Imj.Geo.Discrete
import           Imj.Graphics.ParticleSystem.Design.Types
import           Imj.Graphics.Render
import           Imj.Iteration
import           Imj.Physics.Continuous.Types


{-# INLINABLE drawSystem #-}
drawSystem :: (Draw e, MonadReader e m, MonadIO m)
           => ParticleSystem
           -> Coords Pos
           -- ^ Reference coordinates.
           -> m ()
drawSystem (ParticleSystem points _ (EnvFunctions interaction _) (UpdateSpec _ (Iteration _ frameForNextUpdate))) =
  draw' frameForNextUpdate points interaction

{-# INLINABLE draw' #-}
draw' :: (Draw e, MonadReader e m, MonadIO m)
      => Frame
      -> ParticleTree
      -> (Coords Pos -> InteractionResult)
      -> Coords Pos
      -> m ()
draw' _ (ParticleTree Nothing _ _) _ _   = return ()
draw' _ (ParticleTree (Just []) _ _) _ _ = return ()
draw'
 parentFrame (ParticleTree (Just branches) _ childFrame) interaction r = do
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
            drawGlyph char (sumCoords c r) color)
        $ selectDrawnCoordinates aliveCoordinates
  mapM_ (\child -> draw' relFrame child interaction r) children
