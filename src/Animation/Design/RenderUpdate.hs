{-# LANGUAGE NoImplicitPrelude #-}

module Animation.Design.RenderUpdate
    (
      renderAndUpdate
    ) where

import           Imajuscule.Prelude

import           System.Console.ANSI( Color8Code )

import           Data.Either( partitionEithers )

import           Animation.Types
import           Geo( Coords )
import           Render( RenderState, renderColoredPoints )
import           WorldSize


-- | Updates the state (Tree), computes the points to render from state and
--   pure animation function, renders them and returns an updated animation
--   in which the render function is preapplied the updated state.
renderAndUpdate :: (Iteration -> (Coords -> Location) -> Tree -> Tree)
                -- ^ the pure animation function
                -> (Tree -> StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation))
                -- ^ the IO animation function
                -> (Frame -> Color8Code)
                ->  Tree -> StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation)
renderAndUpdate pureAnim ioAnim colorFunc state@(Tree _ _ branches onWall) step' a@(Animation t i@(Iteration(_, frame)) char _) getLocation r = do
  let step = maybe Initialize (const step') branches
      newState = case step of
        Same -> state
        _    -> pureAnim i getLocation state
      points = getAliveCoordinates newState
      nextAnimation = if null points
                        then
                          Nothing
                        else
                          Just $ case step of
                                Same -> a
                                _    -> Animation t i char $ ioAnim newState
      renderedPoints = case onWall of
        ReboundAnd _ -> points -- every live point is guaranteed to be collision-free
        Traverse  -> filter (( == InsideWorld ) . getLocation) points -- some live points may collide
        Stop      -> error "animation should have stopped"
  renderColoredPoints char renderedPoints (colorFunc frame) r

  return nextAnimation


getAliveCoordinates :: Tree -> [Coords]
getAliveCoordinates (Tree _ _ Nothing _) = []
getAliveCoordinates (Tree _ _ (Just []) _) = []
getAliveCoordinates (Tree _ _ (Just branches) _) =
  let (children, aliveCoordinates) = partitionEithers branches
  in concatMap getAliveCoordinates children ++ aliveCoordinates
