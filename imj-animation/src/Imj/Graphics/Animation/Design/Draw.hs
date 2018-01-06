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
import           Imj.Graphics.Animation.Design.Color
import           Imj.Graphics.Color
import           Imj.Graphics.Render
import           Imj.Iteration
import           Imj.Physics.Continuous.Types

{- | If an 'AnimatedPoint' has no specific 'Char' to be drawn with,
it will be drawn with the 'Char' of the 'Animation'.

Hence, if neither 'AnimatedPoint' nor 'Animation' contain a 'Char', this function
errors.-}
{-# INLINABLE drawAnim #-}
drawAnim :: (Draw e, MonadReader e m, MonadIO m)
           => Animation
           -> Coords Pos
           -- ^ Reference coordinates.
           -> m ()
drawAnim (Animation points _ (EnvFunctions interaction _) (UpdateSpec _ (Iteration _ frameForNextUpdate)) mayChar) =
  draw' frameForNextUpdate mayChar points interaction

{-# INLINABLE draw' #-}
draw' :: (Draw e, MonadReader e m, MonadIO m)
        => Frame
        -> Maybe Char
        -- ^ Default char to use when there is no char specified in the state
        -> AnimatedPoints
        -> (Coords Pos -> InteractionResult)
        -> Coords Pos
        -> m ()
draw' _ _ (AnimatedPoints Nothing _ _) _ _   = return ()
draw' _ _ (AnimatedPoints (Just []) _ _) _ _ = return ()
draw'
 parentFrame mayCharAnim (AnimatedPoints (Just branches) _ childFrame) interaction r = do
  let (children, aliveCoordinates) = partitionEithers branches
      selectDrawnCoordinates =
        filter (\(AnimatedPoint canInteract (VecPosSpeed coords _) _ _) ->
                    case canInteract of
                      -- An alive animated point may collide:
                      DontInteract -> interaction (vec2pos coords) == Stable
                      -- We make the assumption that every alive point is guaranteed to be collision-free.
                      -- Note that when the environment will be dynamic, it will be wrong:
                      Interact -> True)
      relFrame = parentFrame - childFrame
      fg = onBlack $ colorFromFrame (rgb 4 0 0) relFrame
  mapM_ (\(AnimatedPoint _ (VecPosSpeed vc _) mayChar mayColor) -> do
            let char = fromMaybe (error "no char was specified") $ mayChar <|> mayCharAnim
                color = fromMaybe fg mayColor
                c = vec2pos vc
            drawChar char (sumCoords c r) color)
        $ selectDrawnCoordinates aliveCoordinates
  mapM_ (\child -> draw' relFrame mayCharAnim child interaction r) children
