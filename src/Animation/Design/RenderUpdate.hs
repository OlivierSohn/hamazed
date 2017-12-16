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

import           Draw

{- |
Updates and renders 'AnimatedPoints', returns:

* 'Just' the next 'AnimationUpdate' in which the updated 'AnimatedPoints'
  is preapplied to the render function.
* or 'Nothing' if the animation is over
-}
{-# INLINABLE renderAndUpdate #-}
renderAndUpdate :: (Draw e)
                => (Iteration -> (Coords -> Location) -> AnimatedPoints -> AnimatedPoints)
                -- ^ the pure animation function
                -> (AnimatedPoints -> Maybe KeyTime -> AnimationUpdate e -> (Coords -> Location) -> Coords -> ReaderT e IO (Maybe (AnimationUpdate e)))
                -- ^ the IO animation function
                -> (Frame -> LayeredColor)
                ->  AnimatedPoints -> Maybe KeyTime -> AnimationUpdate e -> (Coords -> Location) -> Coords -> ReaderT e IO (Maybe (AnimationUpdate e))
renderAndUpdate pureAnim statelessIOAnim colorFunc state k a@(AnimationUpdate _ (Iteration(_, frame)) mayChar _) getLocation r = do
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
                        -> (Iteration -> (Coords -> Location) -> AnimatedPoints -> AnimatedPoints)
                        -> (Coords -> Location)
                        -> (AnimatedPoints -> Maybe KeyTime -> AnimationUpdate e -> (Coords -> Location) -> Coords -> ReaderT e IO (Maybe (AnimationUpdate e)))
                        -> AnimationUpdate e
                        -> AnimatedPoints
                        -> (AnimationUpdate e, AnimatedPoints)
updateStateAndAnimation k pureAnim getLocation statelessIOAnim a@(AnimationUpdate _ i _ _) state =
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
                -> (Maybe KeyTime -> AnimationUpdate e -> (Coords -> Location) -> Coords -> ReaderT e IO (Maybe (AnimationUpdate e)))
                -> AnimationUpdate e
                -> AnimationUpdate e
updateAnimation Same _ a = a
updateAnimation step r (AnimationUpdate t i c _) =
  update step $ AnimationUpdate t i c r


{-# INLINABLE computeStep #-}
computeStep :: (Draw e) => Maybe KeyTime -> AnimationUpdate e -> AnimatedPoints -> StepType
computeStep mayKey (AnimationUpdate (KeyTime k') _ _ _) (AnimatedPoints _ _ branches _ _) =
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
       -> AnimationUpdate e
       -> AnimationUpdate e
update = \case
            Update -> stepAnimation
            _      -> id

{-# INLINABLE stepAnimation #-}
stepAnimation :: (Draw e)
              => AnimationUpdate e
              -> AnimationUpdate e
stepAnimation (AnimationUpdate t i c f) = AnimationUpdate (addAnimationStepDuration t) (nextIteration i) c f


{-# INLINABLE render #-}
render :: (Draw e)
       => Frame
       -> Maybe Char
       -- ^ default char to use when there is no char specified in the state
       -> AnimatedPoints
       -> (Coords -> Location)
       -> (Frame -> LayeredColor)
       -> Coords
       -> ReaderT e IO Bool
       -- ^ True if at least one animation point is "alive"
render _ _ (AnimatedPoints _ _ Nothing _ _) _ _ _ = return False
render _ _ (AnimatedPoints _ _ (Just []) _ _) _ _ _ = return False
render
 parentFrame mayCharAnim (AnimatedPoints _ childFrame (Just branches) onWall mayCharTree)
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
