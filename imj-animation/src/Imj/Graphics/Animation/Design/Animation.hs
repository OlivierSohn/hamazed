{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Graphics.Animation.Design.Animation
    (
      -- * Animation
      {- | An 'Animation' is updated every 'animationPeriod' to generate 'AnimatedPoint's.

      [Animation function]
      The movements of 'AnimatedPoint's are computed using \(n\) /animation functions/.

      [Level]
      'AnimatedPoint's are organized in \(n\) levels and the motion / aspect of a level \(k\)
      'AnimatedPoint' is defined by the \(k\)th animation function.

      [Mutation]
      An 'AnimatedPoint' can /mutate/, as a result of an interaction with its
      environment. When that happens, it is removed from its level, not animated
      anymore, and in turn, if its level is not the last level, it can give birth
      to 'AnimatedPoint's in the next level.

      Initially, an 'Animation' contains only some level \(1\) 'AnimatedPoint's.
      Higher levels 'AnimatedPoint's are created by the mutation process.
      -}
      Animation
      -- ** Create an Animation
    , mkAnimation
      -- ** Update an Animation
      {- |
      'updateAnimationIfNeeded' updates the 'Animation' if the current time
      is /close enough/ (cf. 'animationUpdateMargin') to the 'Animation' deadline. Else,
       the unmodified 'Animation' is returned. -}
    , updateAnimationIfNeeded
      -- ** Render an Animation
    , renderAnim
    , AnimatedPoint(..)
    , CanInteract(..)
    , InteractionResult(..)
      -- * Utilities
    , earliestDeadline
    ) where


import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)
import           GHC.Show(showString)

import           Data.Either(partitionEithers)

import           Imj.Geo.Discrete
import           Imj.Graphics.Animation.Design.Color
import           Imj.Graphics.Animation.Design.Types
import           Imj.Graphics.Animation.Design.Timing
import           Imj.Graphics.Animation.Design.Update
import           Imj.Graphics.Render
import           Imj.Iteration
import           Imj.Timing

-- | Constructs an 'AnimatedPoints'.
mkAnimatedPoints :: Coords Pos
                 -- ^ Where the first animation should start.
                 -> AnimatedPoints
mkAnimatedPoints c =
  AnimatedPoints 0 c Nothing

data Animation = Animation {
    _animationPoints :: !AnimatedPoints
    -- ^ The current points.
  , _animationUpdate :: !(Frame
                      -> (Coords Pos -> InteractionResult)
                      -> AnimatedPoints
                      -> AnimatedPoints)
    -- ^ The function updating 'AnimatedPoints'.
  , _animationNextUpdateSpec :: !UpdateSpec
    -- ^ The time and iteration of the next update
  , _animationChar :: !(Maybe Char)
    -- ^ The char used to draw animated points when the 'AnimatedPoints'
    -- don't specify one. If 'Nothing', the animation function /must/ specify one
    -- when creating new 'AnimatedPoint's.
}

instance Show Animation where
  showsPrec _ (Animation a _ b c) =
    showString $ "Animation{" ++ show (a,b,c) ++ "}"


mkAnimation :: [Coords Pos -> Frame -> [AnimatedPoint]]
            {- ^ The /animation functions/, where the 'Coords' argument is called
            the /center/.

            During an 'Animation' update, level \(k\) 'AnimatedPoint's will be
            updated using the \(k\)-th animation function in this list-}
            -> KeyTime
            -- ^ When this animation was created.
            -> Speed
            -- ^ Animation discrete speed. Tells by how much the 'Frame', passed
            -- to animation functions, is incremented during an update.
            -> Coords Pos
            -- ^ Will be passed as a /center/ argument to the first
            -- animation function to generate level 1 'AnimatedPoint's.
            -> Maybe Char
            -- ^ The default 'Char' to draw an 'AnimatedPoint' with, if it doesn't
            -- specify one.
            -> Animation
mkAnimation updates t speed pos mayChar =
  let update = updateAnimatedPoints updates
      u = UpdateSpec t (zeroIteration speed)
      points = mkAnimatedPoints pos
  in Animation points update u mayChar


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
                        -> (Coords Pos -> InteractionResult)
                        -- ^ The environmental interaction function
                        -> Animation
                        -- ^ The current animation
                        -> Maybe Animation
                        -- ^ The updated animation, or Nothing if the 'Animation'
                        -- is over.
updateAnimationIfNeeded
 mayK
 interaction
 anim@(Animation points@(AnimatedPoints _ _ branches) update u@(UpdateSpec k it@(Iteration _ frame)) c) =
  let step = computeStep branches k mayK
      newPoints = update frame interaction points
      newUpdateSpec = case step of
                        Update -> UpdateSpec (addDuration animationPeriod k) (nextIteration it)
                        _      -> u
      newAnim = Animation newPoints update newUpdateSpec c
  in case step of
       Same -> Just anim
       _    -> case isActive newAnim of
                 True -> Just newAnim
                 False -> Nothing

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

{- | If some /animation functions/ used to create the 'Animation' don't set the
'Maybe' 'Char' of the 'AnimatedPoint's they create, then 'Animation' /must/
contain a default char, else this function errors.
-}
{-# INLINABLE renderAnim #-}
renderAnim :: (Draw e, MonadReader e m, MonadIO m)
           => Animation
           -> (Coords Pos -> InteractionResult)
           -- ^ The interaction function. Animated points for which this
           -- function returns 'Stable' are drawn. Other animated points are
           -- not drawn because they would overlap with the environment.
           -> Coords Pos
           -- ^ Reference coordinates.
           -> m ()
renderAnim (Animation points _ (UpdateSpec _ (Iteration _ frameForNextUpdate)) mayChar) =
  render' frameForNextUpdate mayChar points

{-# INLINABLE render' #-}
render' :: (Draw e, MonadReader e m, MonadIO m)
        => Frame
        -> Maybe Char
        -- ^ Default char to use when there is no char specified in the state
        -> AnimatedPoints
        -> (Coords Pos -> InteractionResult)
        -> Coords Pos
        -> m ()
render' _ _ (AnimatedPoints _ _ Nothing) _ _   = return ()
render' _ _ (AnimatedPoints _ _ (Just [])) _ _ = return ()
render'
 parentFrame mayCharAnim (AnimatedPoints childFrame _ (Just branches)) interaction r = do
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

isActive :: Animation
         -> Bool
isActive (Animation points _ _ _) =
  hasActivePoints points

hasActivePoints :: AnimatedPoints
                -> Bool
hasActivePoints (AnimatedPoints _ _ Nothing)         = False
hasActivePoints (AnimatedPoints _ _ (Just []))       = False
hasActivePoints (AnimatedPoints _ _ (Just branches)) =
  let (children, activeCoordinates) = partitionEithers branches
      childrenActive = map hasActivePoints children
  in (not . null) activeCoordinates || or childrenActive


-- | Specifies what should be updated.
data StepType = Initialize
              -- ^ Update 'AnimatedPoints'
              | Update
              -- ^ Update 'AnimatedPoints' and 'Iteration'
              | Same
              -- ^ Update 'Iteration'
