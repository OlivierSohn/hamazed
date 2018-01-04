{-#  OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Geo.Continuous.Types
    ( Vec2(..)
    -- Reexports
    , Pos, Vel, Acc
    ) where

import           Imj.Prelude

import           Imj.Geo.Types

-- | Continuous 2d coordinates.
-- In the 'Vec2' name, The @Vec@ comes from
-- <https://en.wikipedia.org/wiki/Vector_graphics vector graphics>.
--
-- We use phantom types 'Pos', 'Vel', 'Acc' to
-- distinguish between a position, a velocity, and an acceleration.
data Vec2 a = Vec2 {
    _vec2X :: {-# UNPACK #-} !Float
  , _vec2Y :: {-# UNPACK #-} !Float
} deriving(Eq, Show, Ord)
