{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Animation.Design.Types
    (
      -- * Animation
      -- | An 'Animation' contains 'AnimatedPoints', and the function to update them:
      Animation(..)
      -- ** Create an Animation
      -- |'mkAnimation' is used to create an 'Animation':
    , mkAnimation
    , AnimationZero(..)
      -- ** Update an Animation
      {- | 'updateAnimationIfNeeded' updates the 'Animation' if the current time
      is /close enough/ (cf. 'animationUpdateMargin') to the 'Animation' deadline. Else,
       the unmodified 'Animation' is returned. -}
    , updateAnimationIfNeeded
      -- ** Render an Animation
      -- | 'renderAnim' renders the 'AnimatedPoints' of the 'Animation'.
    , renderAnim
      -- * Animated points
      {- | Eventhough 'AnimatedPoints' and 'AnimatedPoint' are /internal/ types,
      they are publicly documented here to help understand how animation works. -}
    , AnimatedPoints(..)
    , AnimatedPoint(..)
    , InteractionResult(..)
    , CanInteract(..)
    -- ** AnimatedPoints update
      {-|
      [@final height@] The height of the 'AnimatedPoints' tree that has
      reached its maximal height.

      An animated points at level @n@
      can mutate to an 'AnimatedPoints' by interacting with the environment:

      * if @n == final height@, the new 'AnimatedPoints' will remain empty and the
      tree will stop growing in that direction
      * if @n < final height@, a new animation starts from that location, at level @n+1@.

      Today we support 'AnimatedPoints' of final lengths 1 and 2:

      * 'AnimatedPoints' whose final height == 1 can be updated by 'updateAnimatedPointsUpToLevel1':

          * The level 1 leaves will be updated using a single geometric animation function.

      * 'AnimatedPoints' whose final height == 2 can be updated by 'updateAnimatedPointsUpToLevel2':

          * Level 1 and level 2 leaves will be updated by different geometric animation functions. -}
    , module Imj.Animation.Design.Apply
    , module Imj.Animation.Design.Compose
      -- * Utilities
    , earliestDeadline
    ) where


import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)
import           GHC.Show(showString)

import           Data.Either(partitionEithers)

import           Imj.Animation.Design.Internal.Types
import           Imj.Animation.Design.Timing
import           Imj.Animation.Design.Apply
import           Imj.Animation.Design.Compose
import           Imj.Color.Types
import           Imj.Draw
import           Imj.Geo.Discrete
import           Imj.Iteration
import           Imj.Timing

-- | Constructs an 'AnimatedPoints'.
mkAnimatedPoints :: Coords
                 -- ^ Where the first animation should start.
                 -> AnimatedPoints
mkAnimatedPoints c =
  AnimatedPoints 0 c Nothing


-- | An 'Animation'
data Animation = Animation {
    _animationPoints :: !AnimatedPoints
    -- ^ The current points.
  , _animationUpdate :: !(Iteration
                      -> (Coords -> InteractionResult)
                      -> AnimatedPoints
                      -> AnimatedPoints)
    -- ^ The function updating 'AnimatedPoints'.
  , _animationNextUpdateSpec :: !UpdateSpec
    -- ^ The time and iteration of the next update
  , _animationChar :: !(Maybe Char)
    -- ^ The char used to draw animated points when the 'AnimatedPoints'
    -- don't specify one. If 'Nothing', the '***Geo' functions /must/ specify one
    -- when creating new 'AnimatedPoint's.
}

mkAnimation :: (Iteration -> (Coords -> InteractionResult) -> AnimatedPoints -> AnimatedPoints)
            -- ^ The function updating 'AnimatedPoints'.
            -> KeyTime
            -- ^ When this animation was created.
            -> AnimationZero
            -- ^ Keep or skip the zero frame.
            -> Speed
            -- ^ Animation discrete speed.
            -> Coords
            -- ^ Where the animation starts.
            -> Maybe Char
            -- ^ The default 'Char' to draw an 'AnimatedPoint' with, if it doesn't
            -- specify one.
            -> Animation
mkAnimation update t frameInit speed pos mayChar =
  let u = firstUpdateSpec t frameInit speed
      points = mkAnimatedPoints pos
  in Animation points update u mayChar


instance Show Animation where
  showsPrec _ (Animation a _ b c) =
    showString $ "Animation{" ++ show (a,b,c) ++ "}"

data UpdateSpec = UpdateSpec {
    _updateSpecTime :: !KeyTime
    -- ^ The time at which the update should happen.
  , _updateSpecIteration :: !Iteration
    -- ^ The iteration that will be used in the update.
} deriving(Show)

-- | Returns the earliest animation deadline
earliestDeadline :: [Animation] -> Maybe KeyTime
earliestDeadline animations =
  if null animations
    then
      Nothing
    else
      let getDeadline (Animation _ _ (UpdateSpec k _) _) = k
      in Just $ minimum $ map getDeadline animations

updateAnimationIfNeeded :: Maybe KeyTime
                        -- ^ 'Just' the current 'KeyTime', or 'Nothing'
                        -> (Coords -> InteractionResult)
                        -- ^ The environmental interaction function
                        -> Animation
                        -- ^ The current animation
                        -> Animation
                        -- ^ The updated animation
updateAnimationIfNeeded mayK interaction anim@(Animation points@(AnimatedPoints _ _ branches) update u@(UpdateSpec k iteration) c) =
  let step = computeStep branches k mayK
      newPoints = update iteration interaction points
      newUpdateSpec = case step of
                        Update -> UpdateSpec (addAnimationStepDuration k) (nextIteration iteration)
                        _      -> u
      newAnim = Animation newPoints update newUpdateSpec c
  in case step of
       Same -> anim
       _ -> newAnim

defaultStep :: Maybe [Either AnimatedPoints AnimatedPoint] -> StepType
defaultStep =
  -- if branches is Nothing, it is the first time the animation is rendered / updated
  -- so we need to initialize the state
  maybe Initialize (const Same)

computeStep :: Maybe [Either AnimatedPoints AnimatedPoint]
            -- ^ The root branch.
            -> KeyTime
            -- ^ The animation 'KeyTime'
            -> Maybe KeyTime
            -- ^ 'Just' the current 'KeyTime', or 'Nothing'
            -> StepType
computeStep mayBranches k mayK =
  fromMaybe (defaultStep mayBranches) (computeStep' k mayK)

computeStep' :: KeyTime
            -- ^ The animation 'KeyTime'
            -> Maybe KeyTime
            -- ^ 'Just' the current 'KeyTime', or 'Nothing'
            -> Maybe StepType
computeStep' (KeyTime k') =
  maybe
    Nothing
    (\(KeyTime k) ->
    -- group animations whose keytimes are close
    -- to reduce the amount of renderings needed
    if diffSystemTime k' k < animationUpdateMargin
      then
        Just Update
      else
        Nothing)

-- | If some 'AnimatedPoint' in the 'AnimatedPoints' of the 'Animation' don't
-- have a 'Char' specified to render them, 'Animation' /must/ contain a default
-- one, else this function errors.
{-# INLINABLE renderAnim #-}
renderAnim :: (Draw e, MonadReader e m, MonadIO m)
           => Animation
           -> (Coords -> InteractionResult)
           -- ^ The interaction function. Animated points for which this
           -- function returns 'Stable' are drawn. Other animated points are
           -- not drawn because they would overlap with the environment.
           -> (Frame -> LayeredColor)
           -- ^ Color function
           -> Coords
           -- ^ Reference coordinates.
           -> m Bool
           -- ^ True if at least one animated point is "alive"
renderAnim (Animation points _ (UpdateSpec _ (Iteration _ frameForNextUpdate)) mayChar) =
  render' frameForNextUpdate mayChar points

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
        -- ^ True if at least one animated point is "alive"
render' _ _ (AnimatedPoints _ _ Nothing) _ _ _ = return False
render' _ _ (AnimatedPoints _ _ (Just [])) _ _ _ = return False
render'
 parentFrame mayCharAnim (AnimatedPoints childFrame _ (Just branches))
 interaction colorFunc r = do
  let (children, aliveCoordinates) = partitionEithers branches
      isAlive = (not . null) aliveCoordinates
      selectRenderedCoordinates =
        filter (\(AnimatedPoint canInteract coords _) ->
                    case canInteract of
                      -- An alive animated point may collide:
                      DontInteract -> interaction coords == Stable
                      -- We make the assumption that every alive point is guaranteed to be collision-free.
                      -- Note that when the environment will be dynamic, it will be wrong:
                      Interact -> True)
      relFrame = parentFrame - childFrame
      color = colorFunc relFrame
  mapM_ (\(AnimatedPoint _ c mayChar) -> do
            let char = fromMaybe (error "no char was specified") $ mayChar <|> mayCharAnim
            drawChar char (sumCoords c r) color)
        $ selectRenderedCoordinates aliveCoordinates
  childrenAlive <- mapM (\child -> render' relFrame mayCharAnim child interaction colorFunc r) children
  return $ isAlive || or childrenAlive

-- | Specifies if the zero frame should be skipped or not.
data AnimationZero = WithZero
                   | SkipZero


-- | Specifies what should be updated.
data StepType = Initialize
              -- ^ Update 'AnimatedPoints'
              | Update
              -- ^ Update 'AnimatedPoints' and 'Iteration'
              | Same
              -- ^ Update 'Iteration'

firstUpdateSpec :: KeyTime -> AnimationZero -> Speed -> UpdateSpec
firstUpdateSpec t animZero speed =
  let mayNext = case animZero of
                  WithZero -> id
                  SkipZero -> nextIteration
  in UpdateSpec t (mayNext $ zeroIteration speed)
