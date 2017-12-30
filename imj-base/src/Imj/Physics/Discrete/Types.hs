{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Physics.Discrete.Types
    ( -- * Discrete position and speed
    {- | In a terminal, it is only possible to represent objects at /discrete/
    locations, hence, movable objects have /discrete/ speeds and
    positions. -}
      PosSpeed(..)
    ) where

import           Imj.Prelude

import           Imj.Geo.Discrete.Types

-- | Represents a discrete position and a discrete speed.
data PosSpeed = PosSpeed {
    _posSpeedPos :: !(Coords Pos)
  , _posSpeedSpeed :: !(Coords Vel)
} deriving (Eq, Show)
