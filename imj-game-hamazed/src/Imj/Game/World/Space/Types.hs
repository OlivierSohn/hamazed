{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.World.Space.Types
    ( Space(..)
    , WallType(..)
    , RandomParameters(..)
    , Strategy(..)
    , Material(..)
    , RenderGroup(..)
    , WorldShape(..)
    -- * Reexports
    , module Imj.Geo.Discrete.Types
    ) where

import           Imj.Prelude

import           Data.Matrix( Matrix )

import           Foreign.C.Types( CInt(..) )

import           Imj.Color.Types

import           Imj.Geo.Discrete.Types

data WallType = None
              | Deterministic
              | Random !RandomParameters

data Strategy = StrictlyOneComponent

data RandomParameters = RandomParameters {
    _randomWallsBlockSize :: !Int
  , _randomWallsStrategy :: !Strategy
}

data RenderGroup = RenderGroup {
    _renderGroupCoords :: !Coords
  , _renderGroupColors :: !LayeredColor
  , _renderGroupChar :: !Char
  , _renderGroupCount :: !Int
}

data Space = Space {
    _space :: !(Matrix CInt)
  , _spaceSize :: !Size
  -- ^ Represents the AABB of the space without the border
  , _spaceRender :: ![RenderGroup]
}

data Material = Air
              | Wall
              deriving(Eq, Show)

data WorldShape = Square
                | Rectangle2x1
