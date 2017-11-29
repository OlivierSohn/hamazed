{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

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
import           Timing
import           WorldSize


-- | Updates the state (Tree), computes the points to render from state and
--   pure animation function, renders them and returns an updated animation
--   in which the render function is preapplied the updated state.
renderAndUpdate :: (Iteration -> (Coords -> Location) -> Tree -> Tree)
                -- ^ the pure animation function
                -> (Tree -> Maybe KeyTime -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation))
                -- ^ the IO animation function
                -> (Frame -> Color8Code)
                ->  Tree -> Maybe KeyTime -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation)
renderAndUpdate pureAnim statelessIOAnim colorFunc state@(Tree _ _ _ onWall _) k a@(Animation _ (Iteration(_, frame)) mayChar _) getLocation r = do
  let (nextAnimation, newState) = updateStateAndAnimation k pureAnim getLocation statelessIOAnim a state
      alivePoints = getAliveCoordinates mayChar newState
      -- TODO this is inaccurate : it should take into account the onWall of branches recursively
      -- we should render recursively
      renderedPoints = case onWall of
        ReboundAnd _ -> alivePoints -- every alive point is guaranteed to be collision-free
        Traverse  -> filter (( == InsideWorld ) . getLocation . fst) alivePoints -- some alive points may collide
        Stop      -> error "animation should have stopped"
  renderColoredPoints renderedPoints (colorFunc frame) r

  return $
    if null alivePoints
      then
        Nothing
      else
        Just nextAnimation

updateStateAndAnimation :: Maybe KeyTime
                        -> (Iteration -> (Coords -> Location) -> Tree -> Tree)
                        -> (Coords -> Location)
                        -> (Tree -> Maybe KeyTime -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation))
                        -> Animation
                        -> Tree
                        -> (Animation, Tree)
updateStateAndAnimation k pureAnim getLocation statelessIOAnim a@(Animation _ i _ _) state =
    (nextAnimation, newState)
  where
    step = computeStep k a state
    newState = case step of
      Same -> state
      _    -> pureAnim i getLocation state
    nextAnimation = updateAnimation step (statelessIOAnim newState) a

updateAnimation :: StepType
                -> (Maybe KeyTime -> Animation -> (Coords -> Location) -> RenderState -> IO (Maybe Animation))
                -> Animation
                -> Animation
updateAnimation Same _ a = a
updateAnimation step render (Animation t i c _) =
  update step $ Animation t i c render


computeStep :: Maybe KeyTime -> Animation -> Tree -> StepType
computeStep mayKey (Animation k' _ _ _) (Tree _ _ branches _ _) =
  -- if branches is Nothing, it is the first time the animation is rendered / updated
  -- so we need to initialize the state
  let defaultStep =
        maybe
          Initialize
          (const Same)
            branches
  in  maybe
        defaultStep
        (\k -> if k == k' then Update else defaultStep)
          mayKey


update :: StepType
       -> Animation
       -> Animation
update = \case
            Update -> stepAnimation
            _      -> id

stepAnimation :: Animation
              -> Animation
stepAnimation (Animation t i c f) = Animation (addAnimationStepDuration t) (nextIteration i) c f


getAliveCoordinates :: Maybe Char -> Tree -> [(Coords, Char)]
getAliveCoordinates _ (Tree _ _ Nothing _ _) = []
getAliveCoordinates _ (Tree _ _ (Just []) _ _) = []
getAliveCoordinates mayCharAnim (Tree _ _ (Just branches) _ mayCharTree) =
  let (children, aliveCoordinates) = partitionEithers branches
      mayChar = mayCharTree <|> mayCharAnim
  in case mayChar of
       Nothing -> error "either the pure anim function ar the animation should specify a Just"
       Just char -> concatMap (getAliveCoordinates mayCharAnim) children ++ map (\c -> (c, char)) aliveCoordinates
