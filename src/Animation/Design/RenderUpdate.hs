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
import           Console
import           Geo( Coords )
import           Render
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
renderAndUpdate pureAnim statelessIOAnim colorFunc state k a@(Animation _ (Iteration(_, frame)) mayChar _) getLocation r = do
  let (nextAnimation, newState) = updateStateAndAnimation k pureAnim getLocation statelessIOAnim a state
  isAlive <- render frame mayChar newState getLocation colorFunc r
  return $
    if isAlive
      then
        Just nextAnimation
      else
        Nothing

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
updateAnimation step r (Animation t i c _) =
  update step $ Animation t i c r


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

render :: Frame
       -> Maybe Char
       -- ^ default char to use when there is no char specified in the state
       -> Tree
       -> (Coords -> Location)
       -> (Frame -> Color8Code)
       -> RenderState
       -> IO Bool
       -- ^ True if at least one animation point is "alive"
render _ _ (Tree _ _ Nothing _ _) _ _ _ = return False
render _ _ (Tree _ _ (Just []) _ _) _ _ _ = return False
render parentFrame mayCharAnim (Tree _ childFrame (Just branches) onWall mayCharTree) getLocation colorFunc r = do
  let mayChar = mayCharTree <|> mayCharAnim
  case mayChar of
    Nothing -> error "either the pure anim function ar the animation should specify a Just"
    Just char -> do
      let (children, aliveCoordinates) = partitionEithers branches
          isAlive = (not . null) aliveCoordinates
          renderedCoordinates = case onWall of
            ReboundAnd _ -> aliveCoordinates -- every alive point is guaranteed to be collision-free
            Traverse  -> filter (( == InsideWorld ) . getLocation) aliveCoordinates -- some alive points may collide
            Stop      -> error "animation should have stopped"
          relFrame = parentFrame - childFrame
          color = colorFunc relFrame
      fg <- setRawForeground color
      mapM_ (\c -> renderChar char c r) renderedCoordinates
      restoreForeground fg
      childrenAlive <- mapM (\child -> render relFrame mayCharAnim child getLocation colorFunc r) children
      return $ isAlive || or childrenAlive
