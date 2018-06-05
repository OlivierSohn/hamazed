{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UnboxedTuples, MagicHash #-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveLift #-}

module Imj.Space.Types
    ( Space(..)
    , mkZeroSpace
    , SmallWorldCharacteristics(..)
    , Program
    , User
    , prettyShowSWCharacteristics
    , MatrixVariants(..)
    , MatrixVariantsSpec(..)
    , toVariants
    , toVariantsSpec
    , prettyShowMatrixVariants
    , prettyShowMatrixVariantsSpec
    , humanShowVariants
    , Variation(..)
    , VariationSpec(..)
    , mkVariation
    , mkVariationSpec
    , RotationDetail(..)
    , MkSpaceResult(..)
    , LowerBounds(..)
    , mkLowerBounds
    , SmallWorldRejection(..)
    , SmallWorldComponentsRejection(..)
    , RenderedSpace(..)
    , getSize
    , Material(..)
    , MaterialAndKey(..)
    , isAir
    , materialAndKeyToMaterial
    , DrawGroup(..)
    , Scope(..)
    , Properties(..)
    , mkProperties
    , Statistics(..)
    , prettyShowStats
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

import           Language.Haskell.TH.Syntax(lift)

import           Imj.Prelude
import           Prelude(length)

import           GHC.Storable (readWord16OffPtr, writeWord16OffPtr)
import           GHC.Ptr (Ptr(..))
import           GHC.Word(Word16)

import           Control.Arrow((***))
import           Data.List(unlines, unwords, intercalate)
import           Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NE(map, toList, fromList)
import           Imj.Data.AlmostFloat
import qualified Imj.Data.Matrix.Unboxed as Unboxed
import           Imj.Data.Matrix.Cyclic (Storable(..))
import qualified Imj.Data.Matrix.Cyclic as Cyclic hiding (Storable(..))
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import           Data.Text(pack)
import           Data.Vector.Unboxed.Deriving(derivingUnbox)
import           Data.Vector.Unboxed(Vector)

import           Imj.Data.UndirectedGraph(Vertex)
import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Color.Types

import           Imj.Geo.Discrete.Interleave
import           Imj.Graphics.Text.Render
import           Imj.Graphics.Font
import           Imj.Timing

data DrawGroup = DrawGroup {
    _drawGroupCoords :: {-# UNPACK #-} !(Coords Pos)
  , _drawGroupColors :: {-# UNPACK #-} !LayeredColor
  , _drawGroupGlyph :: {-# UNPACK #-} !Glyph
  , _drawGroupCount :: {-# UNPACK #-} !Int
} deriving(Show)

-- | How to draw the space.
newtype RenderedSpace = RenderedSpace [DrawGroup] -- TODO use an unboxed array to have better memory layout
  deriving (Show)

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

newtype MaterialAndKey = MaterialAndKey Word16 -- -1 for Wall, >= 0 for Air key
  deriving(Generic, Eq, Show)
derivingUnbox "MaterialAndKey"
    [t| MaterialAndKey -> Word16 |]
    [| \(MaterialAndKey m) -> m |]
    [|MaterialAndKey|]
instance Storable MaterialAndKey where -- maps to a Word16
  sizeOf    _ = 2
  alignment _ = 2
  peekElemOff (Ptr a) b = MaterialAndKey <$> readWord16OffPtr (Ptr a) b
  pokeElemOff (Ptr a) b (MaterialAndKey c) = writeWord16OffPtr (Ptr a) b c
  {-# INLINE sizeOf #-}
  {-# INLINE alignment #-}
  {-# INLINE peekElemOff #-}
  {-# INLINE pokeElemOff #-}



{-# INLINE isAir #-}
isAir :: a -> (Word16 -> a) -> MaterialAndKey -> a
isAir no yes (MaterialAndKey x)
 | x < 0x8000 = yes x
 | otherwise = no

{-# INLINE materialAndKeyToMaterial #-}
materialAndKeyToMaterial :: MaterialAndKey -> Material
materialAndKeyToMaterial = isAir Wall (const Air)

data SmallMatInfo = SmallMatInfo {
    _countAirKeys :: !Int
  , _smallMat :: !(Cyclic.Matrix MaterialAndKey)
} deriving(Generic, Show, Eq)
instance NFData SmallMatInfo

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

data SmallWorldCharacteristics a = SWCharacteristics {
    swSize :: {-# UNPACK #-} !Size
    -- ^ Size of the small world
  , swComponentCount :: !ComponentCount
    -- ^ The expected count of connex components.
  , userWallProbability :: !AlmostFloat
    -- ^ User wall proba, in range [0,1]. The /real/ wall proba will then be this value
    -- mapped to the theoretical min / max proba range, computed according to
    -- 'ComponentCount' and 'Size' of the small world.
    --
    -- We use an 'AlmostFloat' to account for numerical errors.
} deriving(Generic, Show, Eq, Ord, Lift)
instance Binary (SmallWorldCharacteristics a)
instance NFData (SmallWorldCharacteristics a)

prettyShowSWCharacteristics :: SmallWorldCharacteristics Program -> String
prettyShowSWCharacteristics (SWCharacteristics (Size (Length h) (Length w)) (ComponentCount nComps) proba) =
  show (h,w) ++ ", " ++ show nComps ++ " component, " ++ show proba ++ " wall."

data MatrixVariants =
    Variants !(NonEmpty Variation) !(Maybe MatrixVariants) -- 'MatrixVariants' applies to every variation produced by NonEmpty Variation
  deriving(Generic, Eq, Ord)
instance Binary MatrixVariants
instance NFData MatrixVariants
instance Show MatrixVariants where
  show = prettyShowMatrixVariants . Just

data MatrixVariantsSpec =
    VariantsSpec !(NonEmpty VariationSpec) !(Maybe MatrixVariantsSpec) -- 'MatrixVariantsSpec' applies to every variation produced by NonEmpty VariationSpec
  deriving(Generic, Eq, Ord)
instance Lift MatrixVariantsSpec where
  lift (VariantsSpec l next) =
    [| VariantsSpec (NE.fromList $(lift (NE.toList l))) $(lift next) |]
instance Binary MatrixVariantsSpec
instance NFData MatrixVariantsSpec
instance Show MatrixVariantsSpec where
  show = prettyShowMatrixVariantsSpec . Just

toVariantsSpec :: MatrixVariants -> MatrixVariantsSpec
toVariantsSpec (Variants l next) = VariantsSpec (NE.map mkVariationSpec l) (fmap toVariantsSpec next)

toVariants :: Size -> MatrixVariantsSpec -> MatrixVariants
toVariants sz (VariantsSpec l next) = Variants (NE.map (mkVariation sz) l) (fmap (toVariants sz) next)

prettyShowMatrixVariants :: Maybe MatrixVariants -> String
prettyShowMatrixVariants =
  maybe
    "No variant"
    go
 where
  go (Variants v mv) =
    intercalate " . " $
      intercalate " + " (map show (NE.toList v)) : maybeToList (fmap go mv)

prettyShowMatrixVariantsSpec :: Maybe MatrixVariantsSpec -> String
prettyShowMatrixVariantsSpec =
  maybe
    "No variant spec"
    go
 where
  go (VariantsSpec v mv) =
    intercalate " . " $
      intercalate " + " (map show (NE.toList v)) : maybeToList (fmap go mv)

humanShowVariants :: Maybe Size -> Maybe MatrixVariants -> String
humanShowVariants sz variations = txt
 where
  random = "using random matrices"
  branchingIn = ", variating in "

  humanShowVariation v@(Rotate (RotationDetail rotationOrder _)) =
    unwords $
      [showVariationDetails v] ++
      maybe [] ((:[]).show) nRotations ++
      ["rotated"]
   where
    nRotations = fmap (Cyclic.countRotations' rotationOrder) sz

  humanShowVariation v@(Interleave r c) =
    unwords [showVariationDetails v, show n, "interleaved"]
   where
    n = nUseful r * nUseful c

  humanShowVariation v@(Modulate from to) =
    unwords [showVariationDetails v, show n, "modulos"]
   where
    n = from-to+1

  humanShowVariates x = unwords [sv x, "variations"]
   where
    sv l = intercalate "+" $ map humanShowVariation l

  txt = random ++ go variations

  go = \case
    Nothing -> " exclusively"
    b@(Just _) ->
      humanShowBranch b ++ "."

  humanShowBranch = maybe
    ""
    (\(Variants variates next) -> branchingIn ++ humanShowVariates (NE.toList variates) ++ humanShowBranch next)

data Variation =
    Rotate !RotationDetail
  | Interleave !InterleaveInfo !InterleaveInfo -- first is for rows, second is for columns
  | Modulate !Int !Int -- from / to, inclusively
  deriving(Generic, Eq, Ord)
instance Binary Variation
instance NFData Variation
instance Show Variation where
  show m@Modulate{} = "M" ++ showVariationDetails m
  show i@Interleave{} = "I" ++ showVariationDetails i
  show r@Rotate{} = unwords ["R", showVariationDetails r]

showVariationDetails :: Variation -> String
showVariationDetails (Rotate detail) = showRotationDetail detail
showVariationDetails (Interleave r c) = show (nUseful r, nUseful c)
showVariationDetails (Modulate from to) = show (from,to)

showRotationDetail :: RotationDetail -> String
showRotationDetail (RotationDetail order (ComponentCount n)) = unwords ["margin-" ++ show n, show order]

data VariationSpec =
    Rotation !RotationDetail
  | Interleaving
  | Modulation
  deriving(Generic, Eq, Ord, Lift)
instance Binary VariationSpec
instance NFData VariationSpec
instance Show VariationSpec where
  show Modulation{} = "M"
  show Interleaving{} = "I"
  show (Rotation detail) = unwords ["R", showRotationDetail detail]

mkVariation :: Size -> VariationSpec -> Variation
mkVariation _ (Rotation x) = Rotate x
mkVariation (Size (Length rows) (Length cols)) Interleaving =
  Interleave (mkInterleaveInfo rows) (mkInterleaveInfo cols)
-- reduce the range, as modulating is less competitive than interleaving, maybe due to memory access patterns which are a lot less linear
mkVariation sz Modulation = Modulate 2 (min 3 $ quot (area sz) 2)

mkVariationSpec :: Variation -> VariationSpec
mkVariationSpec (Rotate x) = Rotation x
mkVariationSpec (Interleave _ _) = Interleaving
mkVariationSpec (Modulate _ _) = Modulation

data RotationDetail = RotationDetail {
    _rotationOrder :: !Cyclic.RotationOrder
  , _distRotate :: !ComponentCount
  -- ^ If the distance between the target number of components and the matrix count of components
  -- is smaller than this value, then we will try rotations.
  --
  -- /Rotated/ variations "preserve" the topology more than /interleaved/ variations, this is the reason why
  -- we don't use this criteria for interleaved rotations.
  --
  -- TODO We could have a function here : depending on the distance, we could
  --
  --  * chose one type of rotation or the other (the choice of Cyclic.RotationOrder could be automated this way)
  --  * chose to rotate "less", i.e take one out of n rotations
} deriving(Generic, Eq, Ord, Lift)
instance Binary RotationDetail
instance NFData RotationDetail
instance Show RotationDetail where
  show (RotationDetail order (ComponentCount n)) = unwords [show n, show order]

data MkSpaceResult r =
    Success r
  | NeedMoreTime
  | Impossible ![Text]
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
  | NCompsRequiredWithMargin {-# UNPACK #-} !ComponentCount
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
instance NFData SmallWorld
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
  deriving(Generic, Eq, Ord, Show, Binary, Num, Enum, Integral, Real, Lift)
instance NFData ComponentCount

data SmallWorldTopology = SmallWorldTopology {
    getConnectedComponents :: [ConnectedComponent]
  , _vertexToSmallMatIndex :: Vertex -> Int -- Int is a matrix index
} deriving(Generic)
instance NFData SmallWorldTopology
instance Show SmallWorldTopology where
  show (SmallWorldTopology a _) = show ("SmallWorldTopology:" :: String,a)

newtype ConnectedComponent = ConnectedComponent (Vector Vertex)
  deriving(Generic, Show)
instance NFData ConnectedComponent


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

data User
data Program

-- | Constant information about the search (dual of 'Statistics')
data Properties = Properties {
    _characteristics :: !(SmallWorldCharacteristics Program)
  , _variants :: !(Maybe MatrixVariants)
  -- ^ Deduced from 'SmallWorldCharacteristics'
  , _lowerbounds :: !(Either Text LowerBounds)
} deriving(Generic, Eq, Ord)
instance Binary Properties
instance NFData Properties
instance Show Properties where
  show = unlines . prettyShowProperties

mkProperties :: SmallWorldCharacteristics Program -> Maybe MatrixVariants -> Properties
mkProperties ch st = Properties ch st $ mkLowerBounds ch

mkLowerBounds :: SmallWorldCharacteristics Program
              -> Either Text LowerBounds
              -- ^ Left is returned when no such world can be generated.
mkLowerBounds (SWCharacteristics sz nComponents _) =
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
  mayMinAir = minCountAirBlocks nComponents sz
  mayMinWall = minCountWallBlocks nComponents sz
  total = area sz
  sizeMsg = pack (show sz) <> " is too small to contain " <> pack (show nComponents)


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
  | h <= 0 || w <= 0 = bool Nothing (Just 0) $ n <= 0
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
} deriving(Generic, Eq)
instance Binary Statistics
instance NFData Statistics
instance Show Statistics where
  show = unlines . prettyShowStats

data DurationStats = Durations {
    totalDuration ::  {-# NOUNPACK #-} !(Time Duration System)
} deriving(Generic, Eq)
instance Binary DurationStats
instance NFData DurationStats
instance Show DurationStats where
  show = unlines . prettyShowDurations

zeroStats :: Statistics
zeroStats = Statistics 0 0 mempty 0 0 0 0 0 0 mkDurationStats

mkDurationStats :: DurationStats
mkDurationStats = Durations zeroDuration

mergeDurations :: DurationStats -> DurationStats -> DurationStats
mergeDurations (Durations a) (Durations a') =
  Durations (a |+| a')

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
    matrixVariations
    lb) =
  "" :
  show ("World generation " ++ humanShowVariants (Just sz) matrixVariations) :
  showInBox strs
 where
  strs =
   either ((:[]) . show) prettyShowLowerBounds lb ++
   showArray
    (Just ("Property","Value"))
    [ ("User-specified N. components", show nComponents')
    , ("User-specified wall probability", show userWallProba)
    , ("Matrices dimensions (h,w)", show (h,w))
    , ("Matrices variations", show matrixVariations)
    ]

prettyShowDurations :: DurationStats -> [String]
prettyShowDurations (Durations total) =
  showArray Nothing
    [ ("Total duration", showTime total)
    ]

prettyShowStats :: Statistics -> [String]
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

prettyShowCCMap :: (String, String) -> Map ComponentCount Int -> [String]
prettyShowCCMap header =
  showArray (Just header) . map (show *** show) . Map.toAscList
