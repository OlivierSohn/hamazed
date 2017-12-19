{-# OPTIONS_HADDOCK prune #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Types for discrete physics.

module Physics.Discrete.Types
    ( PosSpeed(..)
    ) where

import           Imajuscule.Prelude

import           Geo.Discrete.Types

-- | Represents a discrete position and speed.
data PosSpeed = PosSpeed {
    _pos :: !Coords
  , _speed :: !Coords
} deriving (Eq, Show)
