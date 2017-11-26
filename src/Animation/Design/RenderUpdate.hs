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
import           Render( RenderState, renderColored )
import           WorldSize( Location )


-- | Updates the state (Tree), computes the points to render from state and
--   pure animation function, renders them and returns an updated animation
--   in which the render function is preapplied the updated state.
renderAndUpdate :: (Iteration -> (Coords -> Location) -> Tree -> Tree)
                -- ^ the pure animation function
                -> (Tree -> StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation))
                -- ^ the IO animation function
                -> (Frame -> Color8Code)
                ->  Tree -> StepType -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation)
renderAndUpdate pureAnim ioAnim colorFunc state step a@(Animation t i@(Iteration(_, frame)) _) getLocation r = do
  let newState = case step of
        Update -> pureAnim i getLocation state
        Same -> state
      points = getAliveCoordinates newState
      nextAnimation = if null points
                        then
                          Nothing
                        else
                          Just $ case step of
                                Update -> Animation t i $ ioAnim newState
                                Same -> a
  renderColored '|' points (colorFunc frame) r
  -- TODO alternate the animations:
  -- use the number that was shot (handle when multiple are shot?)
  -- $   is funny
  -- ?   is surprising
  -- d   is beautiful
  -- R   is visible
  -- O   is bubbly
  -- o   too
  -- ^   is birds
  -- à   is interesting
  -- {   is birds too
  -- [   is craws
  -- |   is subtle
  -- !   is subtle
  -- :   is nice
  -- ¨   is very light
  -- `   is very light
  -- &   is nice
  -- #   is heavy
  -- @   is round
  -- =   is nice too
  -- -   is light
  -- \   is sharp
  -- /   is sharp
  -- *   is rich

  -- + is too agressive
  -- % is not readable
  return nextAnimation



getAliveCoordinates :: Tree -> [Coords]
getAliveCoordinates (Tree _ _ Nothing) = []
getAliveCoordinates (Tree _ _ (Just [])) = []
getAliveCoordinates (Tree _ _ (Just branches)) =
  let (children, aliveCoordinates) = partitionEithers branches
  in concatMap getAliveCoordinates children ++ aliveCoordinates
