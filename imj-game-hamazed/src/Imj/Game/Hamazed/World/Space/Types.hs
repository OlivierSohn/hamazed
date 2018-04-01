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
    , MaterialAndKey(..)
    , materialAndKeyToMaterial
    , WallDistribution(..)
    , DrawGroup(..)
    , Scope(..)
    , Statistics(..)
    , zeroStats
    , mergeStats
    , mergeMayStats
    , DurationStats(..)
    , BigWorld(..)
    , BigWorldTopology(..)
    , SmallWorld(..)
    , SmallWorldTopology(..)
    , SmallMatInfo(..)
    , ConnectedComponent(..)
    , ComponentCount(..)
    , ComponentIdx(..)
    , NCompsRequest(..)
    , getComponentIndices
    , prettyShowStats
    , unsafeGetMaterial
    , readWorld
    , writeWorld
    -- reexports
    , Glyph
    , module Imj.Geo.Discrete.Types
    ) where

import           Imj.Prelude
import           Prelude(length)

import           Control.Arrow((***))
import           Control.DeepSeq(NFData)
import           Data.Graph(Vertex)
import           Data.List(unlines)
import qualified Imj.Data.Matrix.Unboxed as Unboxed
import qualified Imj.Data.Matrix.Cyclic as Cyclic
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map(toAscList, foldl')
import           Data.Map.Merge.Strict(merge, preserveMissing, zipWithMatched)
import           Data.Vector.Unboxed.Deriving(derivingUnbox)
import           Data.Vector.Unboxed(Vector)
import           Numeric(showFFloat)

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

newtype ComponentIdx = ComponentIdx Int
  deriving(Generic, Enum, Num, Eq, Ord, Show, Binary, Integral, Real)
derivingUnbox "ComponentIdx"
    [t| ComponentIdx -> Int |]
    [| \(ComponentIdx m) -> m |]
    [|ComponentIdx|]


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

newtype MaterialAndKey = MaterialAndKey Int -- -1 for Wall, >= 0 for Air key
  deriving(Generic, Eq, Show)
derivingUnbox "MaterialAndKey"
    [t| MaterialAndKey -> Int |]
    [| \(MaterialAndKey m) -> m |]
    [|MaterialAndKey|]

{-# INLINE materialAndKeyToMaterial #-}
materialAndKeyToMaterial :: MaterialAndKey -> Material
materialAndKeyToMaterial (MaterialAndKey m)
  | m < 0 = Wall
  | otherwise = Air

data SmallMatInfo = SmallMatInfo {
    _countAirKeys :: !Int
  , _smallMat :: !(Cyclic.Matrix MaterialAndKey)
} deriving(Show, Eq)

newtype Space = Space (Unboxed.Matrix Material)
  deriving(Generic, Show, Binary, NFData, Eq)

mkZeroSpace :: Space
mkZeroSpace = Space $ Unboxed.fromLists []

{-# INLINE getSize #-}
getSize :: Space -> Size
getSize (Space m) = Size (Length $ Unboxed.nrows m) (Length $ Unboxed.ncols m)

unsafeGetMaterial :: Coords Pos -> Space -> Material
unsafeGetMaterial (Coords (Coord r) (Coord c)) (Space mat) =
  Unboxed.unsafeGet r c mat

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
  | CC !SmallWorldComponentsRejection !ComponentCount
  deriving(Generic, Show, Eq)
instance Binary SmallWorldRejection
instance NFData SmallWorldRejection

-- | The order in which the constructors are written is also the order in which the checks are done.
data SmallWorldComponentsRejection =
    UnusedFronteers' -- same as UnusedFronteers except that the number of components was computed, too
  | ComponentCountMismatch -- the number of connected components didn't match
  | ComponentsSizesNotWellDistributed -- some components were at least twice as big as others.
  | SpaceNotUsedWellEnough -- Some walls were too thick and didn't bring interesting features to the map.
  deriving(Generic, Show, Eq)
instance Binary SmallWorldComponentsRejection
instance NFData SmallWorldComponentsRejection

data NCompsRequest =
    NCompsNotRequired
  | NCompsRequiredWithPrecision {-# UNPACK #-} !ComponentCount

-- | These 'LowerBounds' can be used to prune the search space, and
-- to adapt user probabilities.
data LowerBounds = LowerBounds {
    minAirBlocks, minWallBlocks :: !Int
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
    getSmallMatrix :: {-# UNPACK #-} !SmallMatInfo
  , _smallTopo :: !SmallWorldTopology
} deriving(Generic)
instance Eq SmallWorld where
  (SmallWorld a _) == (SmallWorld b _) = a == b
instance Show SmallWorld where
  show (SmallWorld a _) = '\n' :
    unlines
      (zipWith (++)
        (showInBox $ writeWorld a)
        (showInBox $ writeGameWorld a))

data BigWorldTopology = BigWorldTopology {
    countComponents :: {-# UNPACK #-} !Int
  , getComponentSize :: ComponentIdx -> Int
  , getEltCoords :: ComponentIdx -> Int -> Coords Pos
} deriving (Generic)
instance Show BigWorldTopology where
  show (BigWorldTopology a _ _) = show ("BigWorldTopology", a)

getComponentIndices :: BigWorldTopology -> [ComponentIdx]
getComponentIndices t = map ComponentIdx [0 .. pred $ countComponents t]


newtype ComponentCount = ComponentCount Int
  deriving(Generic, Eq, Ord, Show, Binary, Num, Enum, Integral, Real)
instance NFData ComponentCount

data SmallWorldTopology = SmallWorldTopology {
    getConnectedComponents :: [ConnectedComponent]
  , _vertexToSmallMatIndex :: Vertex -> Int -- Int is a matrix index
} deriving(Generic)
instance Show SmallWorldTopology where
  show (SmallWorldTopology a _) = show ("SmallWorldTopology:",a)

newtype ConnectedComponent = ConnectedComponent (Vector Vertex)
  deriving(Generic, Show)


readWorld :: [String] -> Unboxed.Matrix Material
readWorld [] = Unboxed.fromList 0 0 []
readWorld l@(s:_)
  | any (/= len) lens = error $ "lengths should all be equal:" ++ show lens
  | otherwise = Unboxed.fromLists $ map (map toMaterial) l
  where
   len = length s
   lens = map length l

writeWorld' :: (Material -> Char) -> SmallMatInfo -> [String]
writeWorld' materialToChar (SmallMatInfo _ mat) = map (map $ materialToChar . materialAndKeyToMaterial) $ Cyclic.toLists mat

writeWorld :: SmallMatInfo -> [String]
writeWorld = writeWorld' toChar

writeGameWorld :: SmallMatInfo -> [String]
writeGameWorld = writeWorld' toGameChar

toMaterial :: Char -> Material
toMaterial 'O' = Air
toMaterial ' ' = Wall
toMaterial x = error $ "Can't parse '" ++ show x ++ "' as a Material"

-- unlike in the game, we draw Air to better see the component shapes
toChar :: Material -> Char
toChar Air = 'O'
toChar Wall = ' '

toGameChar :: Material -> Char
toGameChar Air = ' '
toGameChar Wall = 'Z'

data Statistics = Statistics {
    countInterleavedVariations, countRotationsByIV :: {-# NOUNPACK #-} !Int
  , countRandomMatrices, countGeneratedMatrices :: {-# NOUNPACK #-} !Int
  , countGeneratedGraphsByComponentCount :: !(Map ComponentCount Int)
  , countNotEnoughAir, countNotEnoughWalls, countUnusedFronteers :: {-# NOUNPACK #-} !Int
  , countComponentCountMismatch, countComponentsSizesNotWellDistributed, countSpaceNotUsedWellEnough :: {-# NOUNPACK #-} !Int
  , durations :: {-# NOUNPACK #-} !DurationStats
} deriving(Generic)
instance Binary Statistics
instance NFData Statistics
instance Show Statistics where
  show = prettyShowStats

data DurationStats = Durations {
    randomMatCreation, totalDuration ::  {-# NOUNPACK #-} !(Time Duration System)
} deriving(Generic)
instance Binary DurationStats
instance NFData DurationStats
instance Show DurationStats where
  show = unlines . prettyShowDurations

zeroStats :: Statistics
zeroStats = Statistics 0 0 0 0 mempty 0 0 0 0 0 0 mkDurationStats

mkDurationStats :: DurationStats
mkDurationStats = Durations zeroDuration zeroDuration

mergeDurations :: DurationStats -> DurationStats -> DurationStats
mergeDurations (Durations a b) (Durations a' b') =
  Durations (a |+| a') (b |+| b')

mergeStats :: Statistics -> Statistics -> Statistics
mergeStats (Statistics _ _ z a b c d e f g h i) (Statistics x' y' z' a' b' c' d' e' f' g' h' i') =
  Statistics
    x'
    y'
    (z+z')
    (a+a')
    (merge
      preserveMissing
      preserveMissing
      (zipWithMatched $ \_ elt elt' -> elt + elt')
      b
      b')
    (c+c')
    (d+d')
    (e+e')
    (f+f')
    (g+g')
    (h+h')
    $ mergeDurations i i'

mergeMayStats :: Maybe Statistics -> Maybe Statistics -> Maybe Statistics
mergeMayStats Nothing x = x
mergeMayStats x Nothing = x
mergeMayStats (Just x) (Just y) = Just $ mergeStats x y

prettyShowDurations :: DurationStats -> [String]
prettyShowDurations (Durations onlyMkRandomMat total) =
  showArray Nothing
    [ ("Total duration", showTime total)
    , ("Random mat generation", showRatioAsPercentage ratioRandom)
    ]
 where
  ratioRandom = durationRatio onlyMkRandomMat total

showRatioAsPercentage :: Double -> String
showRatioAsPercentage r =
  let p = r * 100
  in showFFloat (Just 0) p " %"


prettyShowStats :: Statistics -> String
prettyShowStats (Statistics nInterleave nRotations nRandomMatrices nMats ccm notEnoughAir notEnoughWall unusedFronteer ccCountMismatch ccSizesDistribution unusedSpace timings) = unlines $
  "":
  "General world generation statistics:" :
  prettyShowDurations timings ++
  showArray
    (Just ("Stat name","Stat value"))
    [ ("+ N. Random mat", show nRandomMatrices)
    , ("- N. Filtered random (not enough Air)", show notEnoughAir)
    , ("- N. Filtered random (not enough Walls)", show notEnoughWall)
    , ("N. interleaved per random", show nInterleave)
    , ("N. rotations per interleaved", show nRotations)
    , ("Max N. variations per random", show $ nRotations * nInterleave)
    , ("N. mat", show nMats)
    , ("N. mat / N. random mat", show (fromIntegral nMats / fromIntegral nRandomMatrices :: Float))
    , ("- N. Filtered (unused fronteers)", show unusedFronteer)
    , ("N. graphs + component", show $ Map.foldl' (+) 0 ccm)
    , ("- N. Filtered (component count mismatch)", show ccCountMismatch)
    , ("- N. Filtered (wrong size distribution)", show ccSizesDistribution)
    , ("- N. Filtered (space not well used)", show unusedSpace)
    ] ++
  "":
  "Graph generation statistics:" :
  prettyShowCCMap ("N. components", "N. graphs") ccm

prettyShowCCMap :: (String, String) -> Map ComponentCount Int -> [String]
prettyShowCCMap header =
  showArray (Just header) . map (show *** show) . Map.toAscList
