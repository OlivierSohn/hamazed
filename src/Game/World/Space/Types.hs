{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Game.World.Space.Types
    ( Space(..)
    , WallType(..)
    , RandomParameters(..)
    , Strategy(..)
    , Material(..)
    , RenderGroup(..)
    ) where

import           Imajuscule.Prelude

import           System.Console.ANSI( Color8Code(..) )

import           GHC.Generics( Generic )

import           Data.Matrix( Matrix )

import           Foreign.C.Types( CInt(..) )

import           Game.World.Size

import           Geo.Discrete.Types

data WallType = None
              | Deterministic
              | Random RandomParameters

data Strategy = StrictlyOneComponent

data RandomParameters = RandomParameters {
    _randomWallsBlockSize :: !Int
  , _randomWallsStrategy :: !Strategy
}

newtype RenderGroup = RenderGroup (Row, Col, (Color8Code, Color8Code), Char, Int)

data Space = Space {
    _space :: !(Matrix CInt)
  , _spaceSize :: !WorldSize -- ^ represents the aabb of the space without the border
  , _spaceRender :: ![RenderGroup]
}

data Material = Air
              | Wall
              deriving(Generic, Eq, Show)
