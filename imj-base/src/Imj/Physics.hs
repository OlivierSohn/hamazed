
module Imj.Physics
        (-- * Discrete position and speed
        {- | In a terminal, it is only possible to represent objects at /discrete/
        locations, hence, movable objects have /discrete/ speeds and
        positions. -}
          PosSpeed(..)
        -- * Discretely handling collisions
        , mirrorSpeedAndMoveToPrecollisionIfNeeded
        , CollisionStatus(..)
        , firstCollision
        , Mirror(..)
        , shouldMirrorAtomic
         -- * Continuous position and speed
        , VecPosSpeed(..)
        , mkStaticVecPosSpeed
        , posSpeed2VecPosSpeed
          -- * Reexports
        , Vec2, Pos, Vel
        , Location(..)
        ) where

import Imj.Physics.Discrete.Types
import Imj.Physics.Discrete.Collision
import Imj.Physics.Continuous.Types
