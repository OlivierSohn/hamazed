{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Animation.Design.Render
    ( renderAnim
    ) where


import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import           Data.Either(partitionEithers)

import           Imj.Geo.Discrete
import           Imj.Graphics.Animation.Design.Types
import           Imj.Graphics.Animation.Design.Color
import           Imj.Graphics.Render
import           Imj.Iteration

{- | If an 'AnimatedPoint' has no specific 'Char' to be rendered with,
it will be rendered with the 'Char' of the 'Animation'.

Hence, if neither 'AnimatedPoint' nor 'Animation' contain a 'Char', this function
errors.-}
{-# INLINABLE renderAnim #-}
renderAnim :: (Draw e, MonadReader e m, MonadIO m)
           => Animation
           -> Coords Pos
           -- ^ Reference coordinates.
           -> m ()
renderAnim (Animation points _ interaction (UpdateSpec _ (Iteration _ frameForNextUpdate)) mayChar) =
  render' frameForNextUpdate mayChar points interaction

{-# INLINABLE render' #-}
render' :: (Draw e, MonadReader e m, MonadIO m)
        => Frame
        -> Maybe Char
        -- ^ Default char to use when there is no char specified in the state
        -> AnimatedPoints
        -> (Coords Pos -> InteractionResult)
        -> Coords Pos
        -> m ()
render' _ _ (AnimatedPoints Nothing _ _) _ _   = return ()
render' _ _ (AnimatedPoints (Just []) _ _) _ _ = return ()
render'
 parentFrame mayCharAnim (AnimatedPoints (Just branches) _ childFrame) interaction r = do
  let (children, aliveCoordinates) = partitionEithers branches
      selectRenderedCoordinates =
        filter (\(AnimatedPoint canInteract coords _) ->
                    case canInteract of
                      -- An alive animated point may collide:
                      DontInteract -> interaction coords == Stable
                      -- We make the assumption that every alive point is guaranteed to be collision-free.
                      -- Note that when the environment will be dynamic, it will be wrong:
                      Interact -> True)
      relFrame = parentFrame - childFrame
      color = colorFromFrame relFrame
  mapM_ (\(AnimatedPoint _ c mayChar) -> do
            let char = fromMaybe (error "no char was specified") $ mayChar <|> mayCharAnim
            drawChar char (sumCoords c r) color)
        $ selectRenderedCoordinates aliveCoordinates
  mapM_ (\child -> render' relFrame mayCharAnim child interaction r) children
