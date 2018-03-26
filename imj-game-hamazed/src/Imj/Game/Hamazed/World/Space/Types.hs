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
    , mkZeroSpace
    , MkSpaceResult(..)
    , LowerBounds(..)
    , SmallWorldRejection(..)
    , SmallWorldComponentsRejection(..)
    , RenderedSpace(..)
    , getSize
    , Material(..)
    , WallDistribution(..)
    , DrawGroup(..)
    , Scope(..)
    , Statistics(..)
    , zeroStats
    , mergeStats
    , mergeMayStats
    , BigWorld(..)
    , BigWorldTopology(..)
    , SmallWorld(..)
    , SmallWorldTopology(..)
    , ConnectedComponent(..)
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
import           Data.Graph(Vertex)
import           Data.List(unlines)
import           Imj.Data.Matrix.Unboxed(Matrix, ncols, nrows, unsafeGet, fromLists)
import qualified Imj.Data.Matrix.Cyclic as Cyclic(Matrix)
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map(toAscList, foldl')
import           Data.Map.Merge.Strict(merge, preserveMissing, zipWithMatched)
import           Data.Vector.Unboxed.Deriving(derivingUnbox)
import           Data.Vector.Unboxed(Vector)

import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Color.Types

import           Imj.Graphics.Font
import           Imj.Timing
import           Imj.Util

-- | Parameters for random walls creation.
data WallDistribution = WallDistribution {
    blockSize' :: {-# UNPACK #-} !Int
    -- ^ The size of a square wall block.
    --
    -- Note that the smaller the block size, the harder it will be for the algorithm to find
    -- a random world with a single component of air.
  , wallProbability' :: {-# UNPACK #-} !Float -- ^ 1 means only walls, 0 means no walls at all
} deriving(Generic, Show, Eq)
instance Binary WallDistribution
instance NFData WallDistribution

data DrawGroup = DrawGroup {
    _drawGroupCoords :: {-# UNPACK #-} !(Coords Pos)
  , _drawGroupColors :: {-# UNPACK #-} !LayeredColor
  , _drawGroupGlyph :: {-# UNPACK #-} !Glyph
  , _drawGroupCount :: {-# UNPACK #-} !Int
}

-- | How to draw the space.
newtype RenderedSpace = RenderedSpace [DrawGroup] -- TODO use an array to have better memory layout

data Material = Air
              -- ^ In it, ship and numbers can move.
              | Wall
              -- ^ Ship and numbers rebound on 'Wall's.
              deriving(Generic, Eq, Show)
instance Binary Material
instance NFData Material
derivingUnbox "Material"
    [t| Material -> Bool |]
    [| (== Wall) |]
    [| \ i -> if i then Wall else Air|]


newtype Space = Space (Matrix Material)
  deriving(Generic, Show, Binary, NFData, Eq)

mkZeroSpace :: Space
mkZeroSpace = Space $ fromLists []

{-# INLINE getSize #-}
getSize :: Space -> Size
getSize (Space m) = Size (Length $ nrows m) (Length $ ncols m)

unsafeGetMaterial :: Coords Pos -> Space -> Material
unsafeGetMaterial (Coords (Coord r) (Coord c)) (Space mat) =
  unsafeGet r c mat

data Scope = WorldScope !Material
           -- ^ A given 'Material' of the world.
           | NegativeWorldContainer
           -- ^ Excludes the 'World' and its outer view frame.
           deriving(Show)


data MkSpaceResult r =
    Success r
  | NeedMoreTime
  | Impossible [Text]
  deriving (Generic, Show)
instance (Binary r) => Binary (MkSpaceResult r)
instance (NFData r) => NFData (MkSpaceResult r)

-- | Info on small matrix rejection.
--
-- The order in which the constructors are written is also the order in which the checks are done.
data SmallWorldRejection =
    NotEnough !Material -- The random distribution had not enough of some material
                        -- in it to build a world of given 'Size' / 'ComponentCount'.
                        -- (see 'LowerBounds')
  | UnusedFronteers -- some fronteer sides had no 'Air'
  | CC SmallWorldComponentsRejection !ComponentCount
  deriving(Generic, Show, Eq)
instance Binary SmallWorldRejection
instance NFData SmallWorldRejection

-- | The order in which the constructors are written is also the order in which the checks are done.
data SmallWorldComponentsRejection =
    ComponentCountMismatch -- the number of connected components didn't match
  | ComponentsSizesNotWellDistributed -- some components were at least twice as big as others.
  | SpaceNotUsedWellEnough -- Some walls were too thick and didn't bring interesting features to the map.
  deriving(Generic, Show, Eq)
instance Binary SmallWorldComponentsRejection
instance NFData SmallWorldComponentsRejection

-- | These 'LowerBounds' can be used to prune the search space, and
-- to adapt user probabilities.
data LowerBounds = LowerBounds {
    diagnostic :: Either Text ()
    -- ^ 'Left' if the world cannot be built due to topological constraints.
  , mayMinAirBlocks, mayMinWallBlocks :: !(Maybe Int)
    -- ^ The minimium count of air and wall blocks. If any of them is is Nothing, the world is impossible to build.
  , totalBlocks :: {-# UNPACK #-} !Int
  -- ^ The total number of blocks in the world.
} deriving(Generic, Show)

data BigWorld = BigWorld {
    _bigSpace :: {-# UNPACK #-} !Space
  , _bigTopo :: !BigWorldTopology
} deriving(Generic, Show)
instance Eq BigWorld where
  (BigWorld a _) == (BigWorld b _) = a == b

data SmallWorld = SmallWorld {
    getSmallMatrix :: {-# UNPACK #-} !(Cyclic.Matrix Material)
  , _smallTopo :: !SmallWorldTopology
} deriving(Generic, Show)
instance Eq SmallWorld where
  (SmallWorld a _) == (SmallWorld b _) = a == b

data BigWorldTopology = BigWorldTopology {
    countComponents :: {-# UNPACK #-} !Int
  , getComponentSize :: ComponentIdx -> Int
  , getEltCoords :: ComponentIdx -> Int -> Coords Pos
} deriving (Generic)
instance Show BigWorldTopology where
  show (BigWorldTopology a _ _) = show ("BigWorldTopology", a)

getComponentIndices :: BigWorldTopology -> [ComponentIdx]
getComponentIndices t = map ComponentIdx [0 .. pred $ countComponents t]

newtype ComponentIdx = ComponentIdx Int
  deriving(Generic, Enum, Num, Eq, Ord, Show, Binary, Integral, Real)

newtype ComponentCount = ComponentCount Int
  deriving(Generic, Eq, Ord, Show, Binary, Num, Enum, Integral, Real)
instance NFData ComponentCount

data SmallWorldTopology = SmallWorldTopology {
    getConnectedComponents :: [ConnectedComponent]
  , _vertexToCoords :: Vertex -> Coords Pos
  -- ^ Used to get 'ConnectedComponent''s coordinates w.r.t small world
} deriving(Generic)
instance Show SmallWorldTopology where
  show (SmallWorldTopology a _) = show ("SmallWorldTopology:",a)

newtype ConnectedComponent = ConnectedComponent (Vector Vertex)
  deriving(Generic, Show)

data Statistics = Statistics {
    countGeneratedMatrixes :: {-# UNPACK #-} !Int
  , countGeneratedGraphsByComponentCount :: !(Map ComponentCount Int)
  , totalTime :: !(Time Duration System)
} deriving(Generic,Show)
instance Binary Statistics
instance NFData Statistics

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
