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
renderAndUpdate pureAnim ioAnim colorFunc state@(Tree _ _ branches onWall _) step' a@(Animation t i@(Iteration(_, frame)) mayChar _) getLocation r = do
  let step = maybe Initialize (const step') branches
      newState = case step of
        Same -> state
        _    -> pureAnim i getLocation state
      points = getAliveCoordinates mayChar newState
      nextAnimation = if null points
                        then
                          Nothing
                        else
                          Just $ case step of
                                Same -> a
                                _    -> Animation t i mayChar $ ioAnim newState
      renderedPoints = case onWall of
        ReboundAnd _ -> points -- every live point is guaranteed to be collision-free
        Traverse  -> filter (( == InsideWorld ) . getLocation . fst) points -- some live points may collide
        Stop      -> error "animation should have stopped"
  renderColoredPoints renderedPoints (colorFunc frame) r

  return nextAnimation


getAliveCoordinates :: Maybe Char -> Tree -> [(Coords, Char)]
getAliveCoordinates _ (Tree _ _ Nothing _ _) = []
getAliveCoordinates _ (Tree _ _ (Just []) _ _) = []
getAliveCoordinates mayCharAnim (Tree _ _ (Just branches) _ mayCharTree) =
  let (children, aliveCoordinates) = partitionEithers branches
      mayChar = mayCharTree <|> mayCharAnim
  in case mayChar of
       Nothing -> error "either the pure anim function ar the animation should specify a Just"
       Just char -> concatMap (getAliveCoordinates mayCharAnim) children ++ map (\c -> (c, char)) aliveCoordinates
