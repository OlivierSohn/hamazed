{-# LANGUAGE NoImplicitPrelude #-}

module Animation
    ( -- | animations
      simpleExplosion
    , gravityExplosion
    , gravityExplosionThenSimpleExplosion
    , quantitativeExplosionThenSimpleExplosion
    , simpleLaser
    , animatedNumber
    -- | preapplied animations
    , explosion
    , explosion1
    , explosionGravity
    -- | Reexports
    , module Animation.Types
    ) where


import           Imajuscule.Prelude

import           Animation.Design.Animator
import           Animation.Design.Apply
import           Animation.Design.Chain
import           Animation.Design.Geo
import           Animation.Design.RenderUpdate
import           Animation.Types

import           Animation.Color

import           Geo.Continuous

import           Game.World.Laser.Types

import           Timing

{-# INLINABLE simpleLaser #-}
simpleLaser :: (Draw e)
            => LaserRay Actual
            -> Tree
            -> Maybe KeyTime
            -> Animation e
            -> (Coords -> Location)
            -> Coords
            -> ReaderT e IO (Maybe (Animation e))
simpleLaser seg =
  renderAndUpdate' (mkAnimator simpleLaserPure simpleLaser seg)

{-# INLINABLE quantitativeExplosionThenSimpleExplosion #-}
quantitativeExplosionThenSimpleExplosion :: (Draw e)
                                         => Int
                                         -> Tree
                                         -> Maybe KeyTime
                                         -> Animation e
                                         -> (Coords -> Location)
                                         -> Coords
                                         -> ReaderT e IO (Maybe (Animation e))
quantitativeExplosionThenSimpleExplosion number = renderAndUpdate fPure f colorFromFrame
  where
    fPure = chainOnCollision (quantitativeExplosionPure number) (simpleExplosionPure 8)
    f = quantitativeExplosionThenSimpleExplosion number


{-# INLINABLE gravityExplosionThenSimpleExplosion #-}
gravityExplosionThenSimpleExplosion :: (Draw e)
                                    => Vec2
                                    -> Tree
                                    -> Maybe KeyTime
                                    -> Animation e
                                    -> (Coords -> Location)
                                    -> Coords
                                    -> ReaderT e IO (Maybe (Animation e))
gravityExplosionThenSimpleExplosion initialSpeed = renderAndUpdate fPure f colorFromFrame
  where
    fPure = chainOnCollision (gravityExplosionPure initialSpeed) (simpleExplosionPure 8)
    f = gravityExplosionThenSimpleExplosion initialSpeed

{-# INLINABLE gravityExplosion #-}
gravityExplosion :: (Draw e)
                 => Vec2
                 -> Tree
                 -> Maybe KeyTime
                 -> Animation e
                 -> (Coords -> Location)
                 -> Coords
                 -> ReaderT e IO (Maybe (Animation e))
gravityExplosion initialSpeed = renderAndUpdate fPure f colorFromFrame
  where
    fPure = applyAnimation (gravityExplosionPure initialSpeed)
    f = gravityExplosion initialSpeed

{-# INLINABLE animatedNumber #-}
animatedNumber :: (Draw e)
               => Int
               -> Tree
               -> Maybe KeyTime
               -> Animation e
               -> (Coords -> Location)
               -> Coords
               -> ReaderT e IO (Maybe (Animation e))
animatedNumber n =
  renderAndUpdate' (mkAnimator animateNumberPure animatedNumber n)

{-# INLINABLE simpleExplosion #-}
simpleExplosion :: (Draw e)
                => Int
                -> Tree
                -> Maybe KeyTime
                -> Animation e
                -> (Coords -> Location)
                -> Coords
                -> ReaderT e IO (Maybe (Animation e))
simpleExplosion resolution = renderAndUpdate fPure f colorFromFrame
  where
    fPure = applyAnimation (simpleExplosionPure resolution)
    f = simpleExplosion resolution

{-# INLINABLE explosionGravity #-}
explosionGravity :: (Draw e)
                 => Vec2
                 -> Coords
                 -> [Maybe KeyTime -> Animation e -> (Coords -> Location) -> Coords -> ReaderT e IO (Maybe (Animation e))]
explosionGravity speed pos =
  map (`explosionGravity1` pos) $ variations speed

{-# INLINABLE explosionGravity1 #-}
explosionGravity1 :: (Draw e)
                  => Vec2
                  -> Coords
                  -> (Maybe KeyTime -> Animation e -> (Coords -> Location) -> Coords -> ReaderT e IO (Maybe (Animation e)))
explosionGravity1 speed pos =
  gravityExplosion speed (mkAnimationTree pos (ReboundAnd Stop))

{-# INLINABLE explosion #-}
explosion :: (Draw e)
          => Vec2
          -> Coords
          -> [Maybe KeyTime -> Animation e -> (Coords -> Location) -> Coords -> ReaderT e IO (Maybe (Animation e))]
explosion speed pos =
  map (`explosion1` pos) $ variations speed

variations :: Vec2 -> [Vec2]
variations sp =
  map (sumVec2d sp) [ Vec2 0.3     (-0.4)
                    , Vec2 (-0.55) (-0.29)
                    , Vec2 (-0.1)  0.9
                    , Vec2 1.2     0.2]

{-# INLINABLE explosion1 #-}
explosion1 :: (Draw e)
           => Vec2
           -> Coords
           -> (Maybe KeyTime -> Animation e -> (Coords -> Location) -> Coords -> ReaderT e IO (Maybe (Animation e)))
explosion1 speed pos =
  gravityExplosionThenSimpleExplosion speed (mkAnimationTree pos (ReboundAnd $ ReboundAnd Stop))
