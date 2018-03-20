{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Physics.Continuous.Types
    ( VecPosSpeed(..)
    , mkStaticVecPosSpeed
    , posSpeed2VecPosSpeed
    , Vec2, Pos, Vel
    ) where

import           Imj.Prelude

import           Imj.Geo.Continuous.Conversion
import           Imj.Geo.Continuous.Types
import           Imj.Physics.Discrete.Types

{- | Represents a continuous position and a continuous speed.

Note on naming : the @vec@ in the name refers to
<https://en.wikipedia.org/wiki/Vector_graphics vector graphics>. -}
data VecPosSpeed = VecPosSpeed {
    _vecPosSpeedPos :: {-# UNPACK #-} !(Vec2 Pos)
  , _vecPosSpeedSpeed :: {-# UNPACK #-} !(Vec2 Vel)
} deriving (Eq, Show)

-- | Convert from 'PosSpeed' to 'VecPosSpeed'
{-# INLINE posSpeed2VecPosSpeed #-}
posSpeed2VecPosSpeed :: PosSpeed -> VecPosSpeed
posSpeed2VecPosSpeed (PosSpeed pos speed) =
  VecPosSpeed (pos2vec pos) (speed2vec speed)

-- | With a zero speed.
mkStaticVecPosSpeed :: Vec2 Pos -> VecPosSpeed
mkStaticVecPosSpeed p = VecPosSpeed p (Vec2 0 0)
