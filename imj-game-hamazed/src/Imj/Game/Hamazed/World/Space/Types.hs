{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.Hamazed.World.Space.Types
    ( Space(..)
    , Material(..)
    , RandomParameters(..)
    , Strategy(..)
    , DrawGroup(..)
    , Scope(..)
    , module Imj.Geo.Discrete.Types
    ) where

import           Imj.Prelude

import           Data.Matrix( Matrix )

import           Foreign.C.Types( CInt(..) )

import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Color.Types

data Strategy = StrictlyOneComponent
              -- ^ There should be a single connected component of air.
              --
              -- This way, the ship can reach any allowed location without having to go
              -- through 'Wall's.

-- TODO support a world / air ratio (today it is 50 50)
-- | Parameters for random walls creation.
data RandomParameters = RandomParameters {
    _randomWallsBlockSize :: !Int -- TODO support 'Size' to have non-square blocks
    -- ^ The size of a square wall block.
    --
    -- Note that the smaller the block size, the harder it will be for the algorithm to find
    -- a random world with a single component of air.
  , _randomWallsStrategy :: !Strategy
    -- ^ Space characteristics (only /one connected component/ is available for the moment)
}

data DrawGroup = DrawGroup {
    _drawGroupCoords :: !(Coords Pos)
  , _drawGroupColors :: !LayeredColor
  , _drawGroupChar :: !Char
  , _drawGroupCount :: !Int
}

data Space = Space {
    _space :: !(Matrix CInt)
    -- ^ The material matrix.
  , _spaceSize :: !Size
    -- ^ The AABB of the space, excluding the outer border.
  , _spaceDraw :: ![DrawGroup]
    -- ^ How to draw the space.
}

data Material = Air
              -- ^ In it, ship and numbers can move.
              | Wall
              -- ^ Ship and numbers rebound on 'Wall's.
              deriving(Eq, Show)

data Scope = WorldScope !Material
           -- ^ The world, with a 'Material' to say what is the scope of the animation.
--           | ScreenExceptWorldView
--           -- ^ The terminal, excluding the world view.
                deriving(Show)
