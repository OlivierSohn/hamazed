{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Game.World.Space.Types
    ( Space(..)
    , Material(..)
    , RandomParameters(..)
    , Strategy(..)
    , RenderGroup(..)
    , Boundaries(..)
    , module Imj.Geo.Discrete.Types
    ) where

import           Imj.Prelude

import           Data.Matrix( Matrix )

import           Foreign.C.Types( CInt(..) )

import           Imj.Color.Types

import           Imj.Geo.Discrete.Types

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

data RenderGroup = RenderGroup {
    _renderGroupCoords :: !Coords
  , _renderGroupColors :: !LayeredColor
  , _renderGroupChar :: !Char
  , _renderGroupCount :: !Int
}

data Space = Space {
    _space :: !(Matrix CInt)
    -- ^ The material matrix.
  , _spaceSize :: !Size
    -- ^ The AABB of the space, excluding the outer border.
  , _spaceRender :: ![RenderGroup]
    -- ^ How to render the space.
}

data Material = Air
              -- ^ In it, ship and numbers can move.
              | Wall
              -- ^ Ship and numbers rebound on 'Wall's.
              deriving(Eq, Show)

data Boundaries = WorldFrame
                -- ^ Just the world.
                | TerminalWindow
                -- ^ Outside the world, in the terminal.
                | Both
                -- ^ The whole terminal.
                deriving(Show)
