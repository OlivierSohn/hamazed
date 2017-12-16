{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Animation.Design.RenderUpdate
    (
      renderAndUpdate
    ) where

import           Imajuscule.Prelude

import           Data.Either( partitionEithers )

import           Animation.Timing
import           Animation.Types

import           Render

-- | Updates the state (Tree), computes the points to render from state and
--   pure animation function, renders them and returns an updated animation
--   in which the render function is preapplied the updated state.
{-# INLINABLE renderAndUpdate #-}
renderAndUpdate :: (Draw e)
                => (Iteration -> (Coords -> Location) -> Tree -> Tree)
                -- ^ the pure animation function
                -> (Tree -> Maybe KeyTime -> Animation e -> (Coords -> Location) -> Coords -> ReaderT e IO (Maybe (Animation e)))
                -- ^ the IO animation function
                -> (Frame -> LayeredColor)
                ->  Tree -> Maybe KeyTime -> Animation e -> (Coords -> Location) -> Coords -> ReaderT e IO (Maybe (Animation e))
renderAndUpdate pureAnim statelessIOAnim colorFunc state k a@(Animation _ (Iteration(_, frame)) mayChar _) getLocation r = do
  let (nextAnimation, newState) = updateStateAndAnimation k pureAnim getLocation statelessIOAnim a state
  isAlive <- render frame mayChar newState getLocation colorFunc r
  return $
    if isAlive
      then
        Just nextAnimation
      else
        Nothing

{-# INLINABLE updateStateAndAnimation #-}
updateStateAndAnimation :: (Draw e)
                        => Maybe KeyTime
                        -> (Iteration -> (Coords -> Location) -> Tree -> Tree)
                        -> (Coords -> Location)
                        -> (Tree -> Maybe KeyTime -> Animation e -> (Coords -> Location) -> Coords -> ReaderT e IO (Maybe (Animation e)))
                        -> Animation e
                        -> Tree
                        -> (Animation e, Tree)
updateStateAndAnimation k pureAnim getLocation statelessIOAnim a@(Animation _ i _ _) state =
    (nextAnimation, newState)
  where
    step = computeStep k a state
    newState = case step of
      Same -> state
      _    -> pureAnim i getLocation state
    nextAnimation = updateAnimation step (statelessIOAnim newState) a

{-# INLINABLE updateAnimation #-}
updateAnimation :: (Draw e)
                => StepType
                -> (Maybe KeyTime -> Animation e -> (Coords -> Location) -> Coords -> ReaderT e IO (Maybe (Animation e)))
                -> Animation e
                -> Animation e
updateAnimation Same _ a = a
updateAnimation step r (Animation t i c _) =
  update step $ Animation t i c r


{-# INLINABLE computeStep #-}
computeStep :: (Draw e) => Maybe KeyTime -> Animation e -> Tree -> StepType
computeStep mayKey (Animation (KeyTime k') _ _ _) (Tree _ _ branches _ _) =
  let noUpdate =
        maybe
          -- if branches is Nothing, it is the first time the animation is rendered / updated
          -- so we need to initialize the state
          Initialize
          (const Same)
            branches
  in  case mayKey of
        Nothing ->
          noUpdate
        Just (KeyTime k) ->
          -- group animations whose keytimes are close
          -- to reduce the amount of renderings needed
          if diffUTCTime k' k < animationUpdateMargin
            then
              Update
            else
              noUpdate

{-# INLINABLE update #-}
update :: (Draw e)
       => StepType
       -> Animation e
       -> Animation e
update = \case
            Update -> stepAnimation
            _      -> id

{-# INLINABLE stepAnimation #-}
stepAnimation :: (Draw e)
              => Animation e
              -> Animation e
stepAnimation (Animation t i c f) = Animation (addAnimationStepDuration t) (nextIteration i) c f


{-# INLINABLE render #-}
render :: (Draw e)
       => Frame
       -> Maybe Char
       -- ^ default char to use when there is no char specified in the state
       -> Tree
       -> (Coords -> Location)
       -> (Frame -> LayeredColor)
       -> Coords
       -> ReaderT e IO Bool
       -- ^ True if at least one animation point is "alive"
render _ _ (Tree _ _ Nothing _ _) _ _ _ = return False
render _ _ (Tree _ _ (Just []) _ _) _ _ _ = return False
render
 parentFrame mayCharAnim (Tree _ childFrame (Just branches) onWall mayCharTree)
 getLocation colorFunc r = do
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
      mapM_ (\c -> drawChar char (sumCoords c r) color) renderedCoordinates
      childrenAlive <- mapM (\child -> render relFrame mayCharAnim child getLocation colorFunc r) children
      return $ isAlive || or childrenAlive
