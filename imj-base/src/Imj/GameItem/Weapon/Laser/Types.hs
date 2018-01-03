{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.GameItem.Weapon.Laser.Types
    ( LaserRay(..)
    , Theoretical
    , Actual
    , LaserPolicy(..)
    , LaserReach(..)
    ) where

import           Data.Word(Word32)

import           Imj.Geo.Discrete.Types

-- | A laser ray and the direction in which the laser was shot.
data LaserRay a = LaserRay {
    _laserRayDir :: !Direction
    -- ^ The direction in which the laser was shot
  , _laserStart :: !(Coords Pos)
    -- ^ The first point of the laser.
  , _laserLength :: !Word32
    -- ^ The size of the visible portion.
}

-- | The laser ray was computed ignoring obstacles
data Theoretical

-- | The laser ray was computed taking obstacles into account.
data Actual

-- | Tells which obstacles are destroyed by a 'LaserRay' 'Theoretical'
data LaserPolicy = DestroyFirstObstacle
                 -- ^ The first obstacle is destroyed.
                 | DestroyAllObstacles
                 -- ^ All obstacles are destroyed.

-- | The reach of the laser.
data LaserReach = Infinite
