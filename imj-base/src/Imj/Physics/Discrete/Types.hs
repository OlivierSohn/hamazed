{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Imj.Physics.Discrete.Types
    ( PosSpeed(..)
    ) where

import           Imj.Prelude

import           Imj.Geo.Discrete.Types

-- | Represents a discrete position and a discrete speed.
data PosSpeed = PosSpeed {
    getPos :: {-# UNPACK #-} !(Coords Pos)
  , getSpeed :: {-# UNPACK #-} !(Coords Vel)
} deriving (Generic, Eq, Show, Binary)
