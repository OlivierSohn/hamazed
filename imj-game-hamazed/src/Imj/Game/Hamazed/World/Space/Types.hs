{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Imj.Game.Hamazed.World.Space.Types
    ( Space(..)
    , RenderedSpace(..)
    , getSize
    , Material(..)
    , RandomParameters(..)
    , Strategy(..)
    , DrawGroup(..)
    , Scope(..)
    , module Imj.Geo.Discrete.Types
    ) where

import           Imj.Prelude

import           Control.DeepSeq(NFData)
import           Data.Matrix( Matrix, ncols, nrows )

import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Color.Types

data Strategy = OneComponentPerShip
              -- ^ There should be one 'Air' connected component per ship.
              deriving(Generic, Binary, Show, NFData)


-- TODO support a world / air ratio (today it is 50 50)
-- | Parameters for random walls creation.
data RandomParameters = RandomParameters {
    _randomWallsBlockSize :: {-# UNPACK #-} !Int
    -- ^ The size of a square wall block.
    --
    -- Note that the smaller the block size, the harder it will be for the algorithm to find
    -- a random world with a single component of air.
  , _randomWallsStrategy :: {-# UNPACK #-} !Strategy
    -- ^ Space characteristics (only /one connected component/ is available for the moment)
} deriving(Generic, Binary, Show, NFData)

data DrawGroup = DrawGroup {
    _drawGroupCoords :: {-# UNPACK #-} !(Coords Pos)
  , _drawGroupColors :: {-# UNPACK #-} !LayeredColor
  , _drawGroupChar :: {-# UNPACK #-} !Char
  , _drawGroupCount :: {-# UNPACK #-} !Int
}

newtype Space = Space (Matrix Material)

-- | How to draw the space.
newtype RenderedSpace = RenderedSpace [DrawGroup] -- TODO use an array to have better memory layout

{-# INLINE getSize #-}
getSize :: Space -> Size
getSize (Space m) = Size (Length $ nrows m) (Length $ ncols m)

data Material = Air
              -- ^ In it, ship and numbers can move.
              | Wall
              -- ^ Ship and numbers rebound on 'Wall's.
              deriving(Generic, Eq, Show, Binary)

data Scope = WorldScope !Material
           -- ^ A given 'Material' of the world.
           | NegativeWorldContainer
           -- ^ Excludes the 'World' and its outer view frame.
           deriving(Show)
