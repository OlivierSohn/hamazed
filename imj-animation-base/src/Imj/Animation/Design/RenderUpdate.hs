{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Animation.Design.RenderUpdate
    ( renderAndUpdateIfNeeded
    ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import           Data.Either( partitionEithers )

import           Imj.Animation.Design.Timing
import           Imj.Animation.Design.Types
import           Imj.Geo.Discrete
import           Imj.Draw
import           Imj.Iteration
import           Imj.Timing

{- |
Updates (if needed) and renders 'AnimatedPoints'.

If the animation has animation points that are still alive, returns a Just 'AnimationStep'.
-}
{-# INLINABLE renderAndUpdateIfNeeded #-}
renderAndUpdateIfNeeded :: (Draw e, MonadReader e m, MonadIO m)
                => (Iteration -> (Coords -> InteractionResult) -> AnimatedPoints -> AnimatedPoints)
                -- ^ Pure update function
                -> (AnimatedPoints -> Maybe KeyTime -> AnimationStep m -> (Coords -> InteractionResult) -> Coords -> m (Maybe (AnimationStep m)))
                -- ^ This function, with updated 'AnimatedPoints' preapplied,
                -- will be stored in the returned 'AnimationStep'
                -> (Frame -> LayeredColor)
                -- ^ Color function
                ->  AnimatedPoints
                -- ^ Current /state/
                -> Maybe KeyTime
                -- ^ Current time
                -> AnimationStep m
                -- ^ Current 'AnimationStep'
                -> (Coords -> InteractionResult)
                -- ^ Interaction function
                -> Coords
                -- ^ Reference coordinates
                -> m (Maybe (AnimationStep m))
renderAndUpdateIfNeeded pureAnim statelessIOAnim colorFunc state k a@(AnimationStep _ (Iteration _ frame) mayChar _) interaction r = do
  let (nextAnimation, newState) = updateStateAndAnimation k pureAnim interaction statelessIOAnim a state
  isAlive <- render' frame mayChar newState interaction colorFunc r
  return $
    if isAlive
      then
        Just nextAnimation
      else
        Nothing

updateStateAndAnimation :: Maybe KeyTime
                        -> (Iteration -> (Coords -> InteractionResult) -> AnimatedPoints -> AnimatedPoints)
                        -> (Coords -> InteractionResult)
                        -> (AnimatedPoints -> Maybe KeyTime -> AnimationStep m -> (Coords -> InteractionResult) -> Coords -> m (Maybe (AnimationStep m)))
                        -> AnimationStep m
                        -> AnimatedPoints
                        -> (AnimationStep m, AnimatedPoints)
updateStateAndAnimation k pureAnim interaction statelessIOAnim a@(AnimationStep _ i _ _) state =
    (nextAnimation, newState)
  where
    step = computeStep k a state
    newState = case step of
      Same -> state
      _    -> pureAnim i interaction state
    nextAnimation = updateAnimation step (statelessIOAnim newState) a

updateAnimation :: StepType
                -> (Maybe KeyTime -> AnimationStep m -> (Coords -> InteractionResult) -> Coords -> m (Maybe (AnimationStep m)))
                -> AnimationStep m
                -> AnimationStep m
updateAnimation Same _ a = a
updateAnimation step r (AnimationStep t i c _) =
  update step $ AnimationStep t i c r


computeStep :: Maybe KeyTime
            -> AnimationStep m
            -> AnimatedPoints
            -> StepType
computeStep mayKey (AnimationStep (KeyTime k') _ _ _) (AnimatedPoints _ _ branches _ _) =
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
          if diffSystemTime k' k < animationUpdateMargin
            then
              Update
            else
              noUpdate

update :: StepType
       -> AnimationStep m
       -> AnimationStep m
update = \case
            Update -> stepAnimation
            _      -> id

stepAnimation :: AnimationStep m
              -> AnimationStep m
stepAnimation (AnimationStep t i c f) = AnimationStep (addAnimationStepDuration t) (nextIteration i) c f


{-# INLINABLE render' #-}
render' :: (Draw e, MonadReader e m, MonadIO m)
        => Frame
        -> Maybe Char
        -- ^ default char to use when there is no char specified in the state
        -> AnimatedPoints
        -> (Coords -> InteractionResult)
        -> (Frame -> LayeredColor)
        -> Coords
        -> m Bool
        -- ^ True if at least one animation point is "alive"
render' _ _ (AnimatedPoints _ _ Nothing _ _) _ _ _ = return False
render' _ _ (AnimatedPoints _ _ (Just []) _ _) _ _ _ = return False
render'
 parentFrame mayCharAnim (AnimatedPoints _ childFrame (Just branches) onWall mayCharTree)
 interaction colorFunc r = do
  let mayChar = mayCharTree <|> mayCharAnim
  case mayChar of
    Nothing -> error "either the pure anim function ar the animation should specify a Just"
    Just char -> do
      let (children, aliveCoordinates) = partitionEithers branches
          isAlive = (not . null) aliveCoordinates
          renderedCoordinates = case onWall of
            Interact _ -> aliveCoordinates -- every alive point is guaranteed to be collision-free
            DontInteract  -> filter (( == Stable ) . interaction) aliveCoordinates -- some alive points may collide
            Stop      -> error "animation should have stopped"
          relFrame = parentFrame - childFrame
          color = colorFunc relFrame
      mapM_ (\c -> drawChar char (sumCoords c r) color) renderedCoordinates
      childrenAlive <- mapM (\child -> render' relFrame mayCharAnim child interaction colorFunc r) children
      return $ isAlive || or childrenAlive
