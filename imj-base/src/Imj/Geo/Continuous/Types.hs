{-#  OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Geo.Continuous.Types
    ( Vec2(..)
      -- * Reexports
    , module Imj.Geo.Types
    ) where

import           Imj.Prelude

import           Imj.Geo.Types

-- | Continuous 2d coordinates.
data Vec2 a = Vec2 {
    _vec2X :: {-# UNPACK #-} !Float
  , _vec2Y :: {-# UNPACK #-} !Float
} deriving(Eq, Show, Ord)
