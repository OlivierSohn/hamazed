
{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Laser.Types
    (
     -- * Laser representations
     {- | Laser types 'LaserRay' and 'Ray' are parametrized by phantom types
     'Theoretical' and 'Actual' to indicate if the ray was computed taking
     obstacles into account or not.
      -}
      LaserRay(..)
    , Ray(..)
    , Theoretical
    , Actual
    , LaserPolicy(..)
    , LaserType(..)
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

-- | How the laser destroys its targets.
data LaserPolicy = RayDestroysFirst
                 | RayDestroysAll

-- | The reach of the laser.
data LaserType = Infinite
