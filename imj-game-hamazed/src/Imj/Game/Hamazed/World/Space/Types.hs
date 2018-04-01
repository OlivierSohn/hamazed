{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Imj.Game.Hamazed.World.Space.Types
    ( Space(..)
    , mkZeroSpace
    , SmallWorldCharacteristics(..)
    , prettyShowSWCharacteristics
    , SmallWorldCreationStrategy(..)
    , prettyShowSWCreationStrategy
    , MatrixBranchingStrategy(..)
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
    , Properties(..)
    , mkProperties
    , Statistics(..)
    , zeroStats
    , mergeStats
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
    , unsafeGetMaterial
    , readWorld
    , writeWorld
    -- export for tests
    , minCountAirBlocks
    , minCountWallBlocks
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
import qualified Data.Map.Strict as Map
import           Data.Text(pack)
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

data SmallWorldCharacteristics = SWCharacteristics {
    _smallSize :: {-# UNPACK #-} !Size
    -- ^ Size of the small world
  , _componentCount :: !ComponentCount
    -- ^ The expected count of connex components.
  , _userWallProba :: Float
    -- ^ User wall proba, in range [0,1]. The /real/ wall proba will then be this value
    -- mapped to the theoretical min / max proba range, computed according to
    -- 'ComponentCount' and 'Size' of the small world.
} deriving(Generic, Show, Eq, Ord)
instance Binary SmallWorldCharacteristics
instance NFData SmallWorldCharacteristics

prettyShowSWCharacteristics :: SmallWorldCharacteristics -> String
prettyShowSWCharacteristics (SWCharacteristics (Size (Length h) (Length w)) (ComponentCount nComps) proba) =
  show (h,w) ++ ", " ++ show nComps ++ " component, " ++ show proba ++ " wall."

-- TODO update comment when done
-- | (Will soon be) deduced from 'SmallWorldCharacteristics'
data SmallWorldCreationStrategy = SWCreationStrategy {
    _matrixBranchingStrategy :: !MatrixBranchingStrategy
    -- ^ Specifies how matrices are produced, either using only the random number generator, or
    -- by also using deterministic variations of random matrices.
  , _matrixRotationOrder :: !Cyclic.RotationOrder
    -- ^ Specifies the order of rotations.
} deriving(Generic, Show, Eq, Ord)
instance Binary SmallWorldCreationStrategy
instance NFData SmallWorldCreationStrategy

prettyShowSWCreationStrategy :: SmallWorldCreationStrategy -> String
prettyShowSWCreationStrategy (SWCreationStrategy branching rotation) =
  show branching ++ " " ++ show rotation

data MatrixBranchingStrategy =
    Rotate
  | InterleavePlusRotate
  | InterleaveTimesRotate
  deriving(Generic, Show, Eq, Ord)
instance Binary MatrixBranchingStrategy
instance NFData MatrixBranchingStrategy

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
 deriving(Show)

-- | These 'LowerBounds' can be used to prune the search space, and
-- to adapt user probabilities.
data LowerBounds = LowerBounds {
    minAirBlocks, minWallBlocks :: {-# UNPACK #-} !Int
    -- ^ The minimium count of air and wall blocks. If any of them is is Nothing, the world is impossible to build.
  , totalBlocks :: {-# UNPACK #-} !Int
  -- ^ The total number of blocks in the world.
} deriving(Generic, Eq, Ord)
instance Binary LowerBounds
instance NFData LowerBounds
instance Show LowerBounds where
  show l = unlines $
    "" :
    prettyShowLowerBounds l

prettyShowLowerBounds :: LowerBounds -> [String]
prettyShowLowerBounds (LowerBounds minAir minWall total) =
  showArray (Just ("World constraints", "Count"))
    [ ("Total blocks", show total)
    , ("Min Air", show minAir)
    , ("Min Wall", show minWall)
    ]


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
  show (BigWorldTopology a _ _) = show ("BigWorldTopology" :: String, a)

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
  show (SmallWorldTopology a _) = show ("SmallWorldTopology:" :: String,a)

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

-- | Constant information about the search (dual of 'Statistics')
data Properties = Properties !SmallWorldCharacteristics !SmallWorldCreationStrategy !(Either Text LowerBounds)
  deriving(Generic, Eq, Ord)
instance Binary Properties
instance NFData Properties
instance Show Properties where
  show = unlines . prettyShowProperties

mkProperties :: SmallWorldCharacteristics -> SmallWorldCreationStrategy -> Properties
mkProperties ch@(SWCharacteristics sz nComponents' _) st =
  Properties ch st $ mkLowerBounds sz nComponents'

mkLowerBounds :: Size
              -> ComponentCount
              -> Either Text LowerBounds
              -- ^ Left is returned when no such world can be generated.
mkLowerBounds sz n =
  maybe
    (Left sizeMsg)
    (\minAir ->
      maybe
        (Left sizeMsg)
        (\minWall -> bool
          (Left sizeMsg)
          (Right $ LowerBounds minAir minWall total)
          $ minAir + minWall <= total)
        mayMinWall)
    mayMinAir
 where
  mayMinAir = minCountAirBlocks n sz
  mayMinWall = minCountWallBlocks n sz
  total = area sz
  sizeMsg = pack (show sz) <> " is too small to contain " <> pack (show n)


minCountAirBlocks :: ComponentCount -> Size -> Maybe Int
minCountAirBlocks (ComponentCount n) (Size (Length h) (Length w))
  | h <= 0 || w <= 0 = bool Nothing (Just 0) $ n <= 0
  | n <= 0 = Just 0
  | n <= quot (h + w - 1) 2 = Just $ adjustForGoodDistribution $ h + w - n -- L shape with one block interruptions is minimal.
  | n <= quot (h*w + 1) 2 = Just n -- all of size 1, note that at the limit, we have a checkerboard
  | otherwise = Nothing -- impossible to meet the n cc constraint.
  where
   -- 'adjustForGoodDistribution' answers the following problem:
   --
   -- Given a number of components and a minimum count of air blocks,
   -- what is the minimum count of air blocks needed to form a
   -- well-distributed world?
   adjustForGoodDistribution minCount
    | count1 == 1 && count2 > 0 =
     -- we would have some having 1 block, some other having 2,
     -- which is a badly distributed configuration. Hence the answer is 2*n.
        2*n
    | otherwise = minCount
    where
     (count1,count2) = quotRem minCount n

minCountWallBlocks :: ComponentCount -> Size -> Maybe Int
minCountWallBlocks (ComponentCount n) (Size (Length h) (Length w))
  | h <= 0 || w <= 0 = bool Nothing (Just 0) $ n <= 0
  | n <= 0 = Just $ w*h
  | n <= quot (h*w + 1) 2 = Just $ n - 1 -- at the limit, we have a checker board
  | otherwise = Nothing

-- | Varying information about the search (dual of 'Properties')
data Statistics = Statistics {
    countRandomMatrices, countGeneratedMatrices :: {-# NOUNPACK #-} !Int
  , countGeneratedGraphsByComponentCount :: !(Map ComponentCount Int)
  , countNotEnoughAir, countNotEnoughWalls, countUnusedFronteers :: {-# NOUNPACK #-} !Int
  , countComponentCountMismatch, countComponentsSizesNotWellDistributed, countSpaceNotUsedWellEnough :: {-# NOUNPACK #-} !Int
  , durations :: {-# NOUNPACK #-} !DurationStats
} deriving(Generic)
instance Binary Statistics
instance NFData Statistics
instance Show Statistics where
  show = unlines . prettyShowStats

data DurationStats = Durations {
    randomMatCreation, totalDuration ::  {-# NOUNPACK #-} !(Time Duration System)
} deriving(Generic)
instance Binary DurationStats
instance NFData DurationStats
instance Show DurationStats where
  show = unlines . prettyShowDurations

zeroStats :: Statistics
zeroStats = Statistics 0 0 mempty 0 0 0 0 0 0 mkDurationStats

mkDurationStats :: DurationStats
mkDurationStats = Durations zeroDuration zeroDuration

mergeDurations :: DurationStats -> DurationStats -> DurationStats
mergeDurations (Durations a b) (Durations a' b') =
  Durations (a |+| a') (b |+| b')

mergeStats :: Statistics -> Statistics -> Statistics
mergeStats (Statistics z a b c d e f g h i) (Statistics z' a' b' c' d' e' f' g' h' i') =
  Statistics
    (z+z')
    (a+a')
    (safeMerge (+) b b')
    (c+c')
    (d+d')
    (e+e')
    (f+f')
    (g+g')
    (h+h')
    $ mergeDurations i i'

prettyShowProperties :: Properties -> [String]
prettyShowProperties
  (Properties
    (SWCharacteristics sz@(Size (Length h) (Length w)) (ComponentCount nComponents') userWallProba)
    (SWCreationStrategy branchStrategy rotationOrder)
    lb) =
  "" :
  show ("World generation " ++ txt) :
  showInBox strs
 where
  strs =
   either ((:[]) . show) prettyShowLowerBounds lb ++
   showArray
    (Just ("Property","Value"))
    [ ("User-specified N. components", show nComponents')
    , ("User-specified wall probability", show userWallProba)
    , ("Matrices dimensions (h,w)", show (h,w))
    , ("Branching strategy", show branchStrategy)
    , ("Rotation order", show rotationOrder)
    ]

  nInterleaved = Cyclic.countUsefulInterleavedVariations2D sz
  nRotations = Cyclic.countRotations' rotationOrder sz
  {-
  maxMatricesPerRandom = 1 + case branchStrategy of
    Rotate ->
      nRotations
    InterleavePlusRotate ->
      nRotations + nInterleaved
    InterleaveTimesRotate ->
      nRotations * nInterleaved
  -}
  random = "using random matrices"
  branchingIn = ", each of them branching in "
  rotated =
    show nRotations ++ " rotated"
  interleaved =
    show nInterleaved ++ " interleaved"
  variations = " variations"
  txt =
    random ++ case branchStrategy of
      Rotate -> case rotationOrder of
        Cyclic.Order0 -> " exclusively"
        _ -> branchingIn ++ rotated ++ variations
      InterleavePlusRotate ->
        branchingIn ++ rotated ++ " + " ++ interleaved ++ variations
      InterleaveTimesRotate ->
        branchingIn ++ interleaved ++ variations ++ branchingIn ++ rotated ++ variations
    ++ "."

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
  in showFFloat (Just 4) p " %"


prettyShowStats :: Statistics -> [String]
prettyShowStats (Statistics nRandomMatrices nMats ccm notEnoughAir notEnoughWall unusedFronteer ccCountMismatch ccSizesDistribution unusedSpace timings) =
  "" :
  showInBox strs
 where
  strs =
   prettyShowDurations timings ++
   showArray
    (Just ("N. valid matrices"                 , showCount nValid))
    [ ("Randomly generated matrices"           , showCount nRandomMatrices)
    , ("- lack of Air"                         , showFilterCount notEnoughAir)
    , ("- lack of Walls"                       , showFilterCount notEnoughWall)
    , ("Deterministically generated matrices"  , showCount $ nMats - nRandomMatrices)
    , ("- lack of Air on fronteers"            , showFilterCount unusedFronteer)
    , ("- wrong count of components"           , showFilterCount ccCountMismatch)
    , ("- unevenly distributed component sizes", showFilterCount ccSizesDistribution)
    , ("- wasted space"                        , showFilterCount unusedSpace)
    ] ++
   "":
   "Graphs analyses:" :
   prettyShowCCMap ("N. components (lower bound)", "N. graphs") ccm
  nValid = nMats - nFiltered
  nFiltered = notEnoughAir + notEnoughWall + unusedFronteer + ccCountMismatch + ccSizesDistribution + unusedSpace

  showCount :: Int -> String
  showCount 0 = "-"
  showCount x = show x

  showFilterCount = showCount . negate

prettyShowCCMap :: (String, String) -> Map ComponentCount Int -> [String]
prettyShowCCMap header =
  showArray (Just header) . map (show *** show) . Map.toAscList
