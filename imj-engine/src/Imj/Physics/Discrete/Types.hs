{-# OPTIONS_HADDOCK prune #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Types for discrete physics.

module Imj.Physics.Discrete.Types
    ( PosSpeed(..)
    ) where

import           Imj.Prelude

import           Imj.Geo.Discrete.Types

-- | Represents a discrete position and speed.
data PosSpeed = PosSpeed {
    _pos :: !Coords
  , _speed :: !Coords
} deriving (Eq, Show)
