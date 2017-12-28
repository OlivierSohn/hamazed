{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.GameItem.Weapon.Laser.Types
    ( LaserRay(..)
    , Ray(..)
    , Theoretical
    , Actual
    , LaserPolicy(..)
    , LaserReach(..)
    ) where


import           Imj.Geo.Discrete.Types

-- | A laser ray and the direction in which the laser was shot.
data LaserRay a = LaserRay {
    _laserRayDir :: !Direction
    -- ^ The direction in which the laser was shot
  , _laserRaySeg :: !(Ray a)
    -- ^ The laser trajectory.
}

-- | A Laser ray
newtype Ray a = Ray Segment

-- | The laser ray was computed ignoring obstacles
data Theoretical

-- | The laser ray was computed taking obstacles into account.
data Actual

-- | Tells which obstacles are destroyed on the 'Segment' of 'Ray' 'Theoretical'
data LaserPolicy = DestroyFirstObstacle
                 -- ^ The first obstacle is destroyed.
                 | DestroyAllObstacles
                 -- ^ All obstacles are destroyed.

-- | The reach of the laser.
data LaserReach = Infinite
