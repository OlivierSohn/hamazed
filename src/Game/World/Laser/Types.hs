
{-# LANGUAGE NoImplicitPrelude #-}

module Game.World.Laser.Types
    ( LaserRay(..)
    , Theoretical
    , Actual
    , Ray(..)
    , LaserPolicy(..)
    , LaserType(..)
    ) where


import           Geo.Discrete.Types

data LaserRay a = LaserRay {
    _laserRayDir :: !Direction
  , _laserRaySeg :: !(Ray a)
}

newtype Ray a = Ray Segment

data Theoretical -- with no obstacle
data Actual      -- with obstacles

data LaserPolicy = RayDestroysFirst | RayDestroysAll
data LaserType = Infinite
