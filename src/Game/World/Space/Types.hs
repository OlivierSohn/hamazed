{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Game.World.Space.Types
    ( Space(..)
    , WallType(..)
    , RandomParameters(..)
    , Strategy(..)
    , Material(..)
    , RenderGroup(..)
    , WorldShape(..)
    , WorldSize(..)
    , maxDim
    , Width(..)
    , Height(..)
    -- | reexports
    , module Geo.Discrete.Types
    ) where

import           Imajuscule.Prelude

import           GHC.Generics( Generic )

import           Data.Matrix( Matrix )

import           Foreign.C.Types( CInt(..) )

import           Color.Types

import           Geo.Discrete.Types

data WallType = None
              | Deterministic
              | Random RandomParameters

data Strategy = StrictlyOneComponent

data RandomParameters = RandomParameters {
    _randomWallsBlockSize :: !Int
  , _randomWallsStrategy :: !Strategy
}

data RenderGroup = RenderGroup {
    _renderGroupCoords :: !Coords
  , _renderGroupColors :: !LayeredColor
  , _renderGroupChar :: !Char
  , _renderGroupCount :: Int
}

data Space = Space {
    _space :: !(Matrix CInt)
  , _spaceSize :: !WorldSize -- ^ represents the aabb of the space without the border
  , _spaceRender :: ![RenderGroup]
}

data Material = Air
              | Wall
              deriving(Generic, Eq, Show)

data WorldShape = Square
                | Rectangle2x1

newtype WorldSize = WorldSize Coords deriving(Eq, Show)

maxDim :: WorldSize -> Int
maxDim (WorldSize (Coords (Row rs) (Col cs))) = max rs cs

newtype Width = Width Int
newtype Height = Height Int
