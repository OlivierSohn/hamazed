{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Imj.Game.Hamazed.World.Space.Types
    ( Space(..)
    , RenderedSpace(..)
    , getSize
    , Material(..)
    , MaterialMatrix(..)
    , RandomParameters(..)
    , DrawGroup(..)
    , Scope(..)
    , Statistics(..)
    , zeroStats
    , mergeStats
    , mergeMayStats
    , BigWorldTopology(..)
    , ComponentCount(..)
    , ComponentIdx(..)
    , getComponentIndices
    , prettyShowStats
    , unsafeGetMaterial
    -- reexports
    , Glyph
    , module Imj.Geo.Discrete.Types
    ) where

import           Imj.Prelude

import           Control.Arrow((***))
import           Control.DeepSeq(NFData)
import           Data.List(unlines)
import           Imj.Data.Matrix.Unboxed(Matrix, ncols, nrows, unsafeGet)
import           Data.Map(Map)
import qualified Data.Map as Map(toAscList, foldl')
import           Data.Map.Merge.Strict(merge, preserveMissing, zipWithMatched)
import           Data.Vector.Unboxed.Deriving(derivingUnbox)

import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Color.Types

import           Imj.Graphics.Font
import           Imj.Timing
import           Imj.Util

-- | Parameters for random walls creation.
data RandomParameters = RandomParameters {
    _randomWallsBlockSize :: {-# UNPACK #-} !Int
    -- ^ The size of a square wall block.
    --
    -- Note that the smaller the block size, the harder it will be for the algorithm to find
    -- a random world with a single component of air.
  , _wallProbability :: {-# UNPACK #-} !Float -- ^ 1 means only walls, 0 means no walls at all
} deriving(Generic, Show, Eq)
instance Binary RandomParameters
instance NFData RandomParameters

data DrawGroup = DrawGroup {
    _drawGroupCoords :: {-# UNPACK #-} !(Coords Pos)
  , _drawGroupColors :: {-# UNPACK #-} !LayeredColor
  , _drawGroupGlyph :: {-# UNPACK #-} !Glyph
  , _drawGroupCount :: {-# UNPACK #-} !Int
}

newtype Space = Space (Matrix Material)

-- | How to draw the space.
newtype RenderedSpace = RenderedSpace [DrawGroup] -- TODO use an array to have better memory layout

{-# INLINE getSize #-}
getSize :: Space -> Size
getSize (Space m) = Size (Length $ nrows m) (Length $ ncols m)

newtype MaterialMatrix = MaterialMatrix [[Material]] -- TODO ByteString would use 3 * 64 times less memory
  deriving(Generic, Show, Binary)

data Material = Air
              -- ^ In it, ship and numbers can move.
              | Wall
              -- ^ Ship and numbers rebound on 'Wall's.
              deriving(Generic, Eq, Show)
instance Binary Material
derivingUnbox "Material"
    [t| Material -> Bool |]
    [| (== Wall) |]
    [| \ i -> if i then Wall else Air|]

unsafeGetMaterial :: Coords Pos -> Space -> Material
unsafeGetMaterial (Coords (Coord r) (Coord c)) (Space mat) =
  unsafeGet r c mat

data Scope = WorldScope !Material
           -- ^ A given 'Material' of the world.
           | NegativeWorldContainer
           -- ^ Excludes the 'World' and its outer view frame.
           deriving(Show)


data BigWorldTopology = BigWorldTopology {
    countComponents :: {-# UNPACK #-} !Int
  , getComponentSize :: ComponentIdx -> Int
  , getEltCoords :: ComponentIdx -> Int -> Coords Pos
}

getComponentIndices :: BigWorldTopology -> [ComponentIdx]
getComponentIndices t = map ComponentIdx [0 .. pred $ countComponents t]

newtype ComponentIdx = ComponentIdx Int
  deriving(Generic, Eq, Ord, Show, Binary)


newtype ComponentCount = ComponentCount Int
  deriving(Generic, Eq, Ord, Show, Binary, Num, Enum)

data Statistics = Statistics {
    countGeneratedMatrixes :: {-# UNPACK #-} !Int
  , countGeneratedGraphsByComponentCount :: !(Map ComponentCount Int)
  , totalTime :: !(Time Duration System)
} deriving(Generic,Show)
instance Binary Statistics

zeroStats :: Statistics
zeroStats = Statistics 0 mempty zeroDuration

mergeStats :: Statistics -> Statistics -> Statistics
mergeStats (Statistics a b c) (Statistics a' b' c') =
  Statistics
    (a+a')
    (merge
      preserveMissing
      preserveMissing
      (zipWithMatched $ \_ x y -> x + y)
      b
      b')
    $ c |+| c'

mergeMayStats :: Maybe Statistics -> Maybe Statistics -> Maybe Statistics
mergeMayStats Nothing x = x
mergeMayStats x Nothing = x
mergeMayStats (Just x) (Just y) = Just $ mergeStats x y

prettyShowStats :: Statistics -> String
prettyShowStats (Statistics nMats ccm dt) = unlines $
  "":
  "General world generation statistics:" :
  ("Computation time:" ++ show dt):
  showArray
    (Just ("Stat name","Stat value"))
    [ ("N. generated matrices", show nMats)
    , ("N. generated graphs", show $ Map.foldl' (+) 0 ccm)] ++
  "":
  "Graph generation statistics:" :
  prettyShowCCMap ("N. components", "N. graphs") ccm

prettyShowCCMap :: (String, String) -> Map ComponentCount Int -> [String]
prettyShowCCMap header =
  showArray (Just header) . map (show *** show) . Map.toAscList
