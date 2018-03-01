{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Graphics.ParticleSystem.Design.UpdateParticles
    ( updateParticles
    ) where


import           Imj.Prelude

import           Data.List( length )
import           Data.Maybe( fromMaybe )

import           Imj.Geo.Continuous
import           Imj.Geo.Discrete
import           Imj.Graphics.ParticleSystem.Design.Types
import           Imj.Graphics.Color
import           Imj.Graphics.Font
import           Imj.Iteration
import           Imj.Physics.Discrete.Collision
import           Imj.Physics.Continuous.Types


{- | Given a length \( n \) list of particle functions, updates the \( n \)
first levels of a 'ParticleTree' using one particle function per level.

An 'Particle' at level \( k <= n \) can mutate to a 'ParticleTree' by
interacting with its environment:

* if \( k = n \) , the new 'ParticleTree' will remain empty.
* if \( k < n \), the new 'ParticleTree' will be populated by 'ParticleTree'
using the \( k+1 \)th particle function.
-}
updateParticles :: [VecPosSpeed -> Frame -> [Particle]]
                -- ^ The particle function at index @i@ updates
                -- 'ParticleTree' at level @i@.
                -> EnvFunctions
                -> Frame
                -- ^ Current iteration
                -> ParticleTree
                -> ParticleTree
updateParticles [] _ _ aps = aps
updateParticles
 (f:fs)
 envFuncs@(EnvFunctions _ distance)
 globalFrame
 original@(ParticleTree branches center@(VecPosSpeed cPos _) startFrame) =
  let relativeFrame = globalFrame - startFrame
      branchesLevel1Mutated = updatePointsAndMutateIfNeeded f center relativeFrame envFuncs branches
      newBranches = map (\case
                            -- recurse for the 'ParticleTree's
                            Left aps -> Left $ updateParticles fs envFuncs relativeFrame aps
                            -- the 'Particle's are already up-to-date due to updatePointsAndMutateIfNeeded:
                            Right ap -> Right ap
                            ) branchesLevel1Mutated
  in if isNothing branches && distance cPos == TooFar
       then
         -- do not develop this branch, as its center is too far.
         original
       else
         ParticleTree (Just newBranches) center startFrame


-- | Doesn't change the existing /level 1/ 'ParticleTree's, but can convert some
-- 'Particle's to 'ParticleTree's.
updatePointsAndMutateIfNeeded :: (VecPosSpeed -> Frame -> [Particle])
                              -- ^ Geometric particle function
                              -> VecPosSpeed
                              -> Frame
                              -- ^ Relative frame
                              -> EnvFunctions
                              -> Maybe [Either ParticleTree Particle]
                              -- ^ Current branches
                              -> [Either ParticleTree Particle]
                              -- ^ Updated branches
updatePointsAndMutateIfNeeded
 mkParticles root@(VecPosSpeed rootPos _) frame envFunctions@(EnvFunctions interaction _) branches =
  let particles =
        mkParticles
          (assert (interaction (vec2pos rootPos) == Stable || error (show rootPos)) root)
          frame
      space = textGlyph ' '
      defaultState = map (\(Particle canInteract _ _ _) ->
                            Right $ Particle canInteract root space whiteOnBlack)
                        particles
      previousState = fromMaybe defaultState branches
      -- if previousState contains only Left(s), the animation does not need to be computed.
      -- I wonder if lazyness takes care of that or not?
  in combine particles previousState frame envFunctions

combine :: [Particle]
        -> [Either ParticleTree Particle]
        -> Frame
        -> EnvFunctions
        -> [Either ParticleTree Particle]
combine particles previousState frame interaction =
  let check = allowedPointCountVariation (length previousState) (length particles)
  in  assert check $
      zipWith
        (combineParticles interaction frame)
        particles previousState

combineParticles :: EnvFunctions
                 -> Frame
                 -> Particle
                 -> Either ParticleTree Particle
                 -> Either ParticleTree Particle
combineParticles (EnvFunctions interaction distance) frame particle@(Particle onWall cur@(VecPosSpeed curPos _) _ _) =
  either
    Left
    (\(Particle prevOnWall prev _ _) ->
      case assert (prevOnWall == onWall) onWall of
        DontInteract -> Right particle
        Interact ->
          case distance curPos of
            TooFar -> Left $ ParticleTree Nothing cur frame
            DistanceOK ->
              -- Since the environment is static, we can drop the first point of the trajectory,
              -- because we know by design that it doesn't collide. This assert verifies that:
              maybe
                 (Right $ assert (interaction (vec2pos curPos) == Stable) particle)
                 (\preCollisionMirrored ->
                    Left $ ParticleTree Nothing preCollisionMirrored frame)
                 $ getCoordsBeforeMutation' prev cur interaction
    )


{- | To improve (small) rebounds realism:

* when a speed mirroring occurs on one
coordinate, we compute \(intersect\), the intersection of the (continuous) trajectory with the
(continuous) fronteer between pixels that collide and pixels that don't collide.

    * The coordinate of \(intersect\) in the same direction as the fronteer
      will be used, but the coordinate in the othogonal direction will be dropped,
      replaced by the one of the pixel-centered precollision point.

* When a speed mirroring occurs on both coordinates, we use the pixel-centered
precollision point.
-}
getCoordsBeforeMutation' :: VecPosSpeed
                         -> VecPosSpeed
                         -> (Coords Pos -> InteractionResult)
                         -> Maybe VecPosSpeed
getCoordsBeforeMutation'
 (VecPosSpeed prevVecCoords' prevSpeed)
 (VecPosSpeed vecCoords curSpeed)
 interaction =
  refine <$> getCoordsBeforeMutation trajectory interaction 0
 where
  prevVecCoords = assert (interaction (vec2pos prevVecCoords') == Stable) prevVecCoords'
  trajectory = bresenham $ mkSegment (vec2pos prevVecCoords) (vec2pos vecCoords)
  refine (i, mirror, preCollision, afterPreCollision) =
    let -- precollision position corresponds to index i on trajectory:
        -- if i == 0, speed is prevSpeed
        -- if i == pred length, speed is curSpeed
        --   Note that this case is impossible here because a precolliding
        --   index is always <= length - 2
        (Vec2 speedX speedY)
          | i == maxIdx = error "a precolliding index is always <= length - 2" -- curSpeed
          | otherwise = sumVec2d prevSpeed $ scalarProd progress $ diffVec2d curSpeed prevSpeed
          where
            maxIdx = pred $ length trajectory
            progress = fromIntegral i / fromIntegral maxIdx -- maxIdx is never 0 when this is fully evaluated
        vPosPreCol@(Vec2 precolX precolY) = pos2vec $ assert (interaction preCollision == Stable) preCollision
        (Vec2 afterPrecolX afterPrecolY) = pos2vec afterPreCollision
    in case mirror of
        MirrorAll ->
          VecPosSpeed vPosPreCol $ Vec2 (negate speedX) (negate speedY)
        _ ->
          VecPosSpeed pos $ case mirror of
                      MirrorCol -> Vec2 (negate speedX) speedY
                      MirrorRow -> Vec2 speedX          (negate speedY)
                      MirrorAll -> error "logic error"
          where
            pos =
              let -- fronteerLine describes the fronteer between pixels
                  -- inside and outside the world.
                  fronteerLine = case mirror of
                    MirrorCol -> VerticalPxFronteer   $ (precolX + afterPrecolX) / 2
                    MirrorRow -> HorizontalPxFronteer $ (precolY + afterPrecolY) / 2
                    MirrorAll -> error "logic error"
              in
                 -- We approximate the real trajectory by a straight line and compute
                 -- the intersection with the pixel-fronteer line, based on precollision and mirroring:
                 maybe
                  vPosPreCol
                  (\(Vec2 fronteerX fronteerY) ->
                      let adjustedIntersection = case mirror of
                            MirrorCol -> Vec2 precolX fronteerY
                            MirrorRow -> Vec2 fronteerX precolY
                            MirrorAll -> error "logic error"
                      in if interaction (vec2pos adjustedIntersection) == Stable
                          then adjustedIntersection
                          -- Collides when the position just after precollision
                          -- is very different from vecCoords, which could happen
                          -- for parabollas. In that case, fallback to precollision:
                          else vPosPreCol
                  ) $ fronteerIntersection prevVecCoords vecCoords fronteerLine

data PixelFronteer = HorizontalPxFronteer !Float
                   | VerticalPxFronteer !Float

-- | Find an intersection, if any, between a line and a pixel fronteer.
fronteerIntersection :: Vec2 Pos
                     -- ^ Point1 of the line
                     -> Vec2 Pos
                     -- ^ Point2 of the line
                     -> PixelFronteer
                     -> Maybe (Vec2 Pos)
fronteerIntersection (Vec2 x1 y1) (Vec2 x2 y2)
  | x1 == x2 = \case
    (VerticalPxFronteer   _) -> Nothing
    (HorizontalPxFronteer y) -> Just $ Vec2 x2 y
  | y1 == y2 = \case
    (VerticalPxFronteer   x) -> Just $ Vec2 x y2
    (HorizontalPxFronteer _) -> Nothing
  | otherwise = \case
    (VerticalPxFronteer   x) -> Just $ Vec2 x $ slope * x + b
    (HorizontalPxFronteer y) -> Just $ Vec2 ((y - b)/slope) y
 where
  -- We use the 2D line equation \(y = slope * x + b\)
  slope = (y2-y1)/(x2-x1)
  b = y1 - slope * x1


-- | The first point of the trajectory is expected to be stable.
getCoordsBeforeMutation :: [Coords Pos]
                        -- ^ The trajectory
                        -> (Coords Pos -> InteractionResult)
                        -- ^ The interaction function
                        -> Int
                        -- ^ The pre-collision index sofar (non recursive caller should pass 0)
                        -> Maybe (Int, Mirror, Coords Pos, Coords Pos)
                        -- ^ (index of precollision, mirroring, precollision, after precollision)
getCoordsBeforeMutation [] _ _ = error "not supposed to happen"
getCoordsBeforeMutation [_] _ _ = Nothing
getCoordsBeforeMutation (a:as@(b:_)) interaction i =
  maybe
    (getCoordsBeforeMutation as interaction (succ i))
    (\mirror -> Just (i, mirror, a, b))
    $ shouldMirrorAtomic colFunc a b
 where
  colFunc =
    interaction >>> \case
      Mutation -> OutsideWorld
      Stable -> InsideWorld

{- | Verifies that the variation in number of points is allowed:

The number of 'Particle's generated by a particular /particle function/ should be constant,
or change from non-zero to 0 to signify the end of the production. -}
allowedPointCountVariation :: Int
                           -- ^ From
                           -> Int
                           -- ^ To
                           -> Bool
allowedPointCountVariation from to =
  to == from || to == 0
