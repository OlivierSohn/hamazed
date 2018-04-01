{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.Game.Hamazed.World.Space
    ( Space
    , Material(..)
    , mkEmptySpace
    , mkFilledSpace
    , mkRandomlyFilledSpace
    , location
    , distanceToSpace
    , Scope(..)
    , mkRandomPosSpeed
    , getBigCoords
    , countBigCCElts
    , OverlapKind(..)
    , randomCCCoords
    -- for tests
    , withInterleaving
    , unsafeMkSmallMat
    , mkSmallWorld
    , mkSmallMat
    , tryRotationsIfAlmostMatches
    , matchTopology
    , getComponentCount
    , TopoMatch
    ) where

import           Imj.Prelude

import           Data.Either(partitionEithers, isLeft)
import qualified Imj.Data.Graph as Directed(graphFromSortedEdges, componentsN)
import qualified Imj.Data.UndirectedGraph as Undirected(Graph, Vertex, componentsN)
import qualified Data.Array.Unboxed as UArray(Array, array, (!))
import           Data.List(length, sortOn)
import qualified Data.List as List (foldl')
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import           Data.Set(Set)
import qualified Data.Set as Set(size, empty, fromList, toList, union)
import           Data.Tree(flatten)
import qualified Data.Vector.Mutable as BVM (new, write)
import qualified Data.Vector.Unboxed.Mutable as VM (new, write)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as BV
import           System.Random.MWC(GenIO, uniform, uniformR)

import qualified Imj.Data.Matrix.Unboxed as Unboxed
import qualified Imj.Data.Matrix.Cyclic as Cyclic
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Graphics.Class.Positionable
import           Imj.Physics.Discrete
import           Imj.Timing
import           Imj.Util

-- | Creates a 'PosSpeed' from a position,
-- moves to precollision and mirrors speed if a collision is detected for
-- the next step (see 'mirrorSpeedAndMoveToPrecollisionIfNeeded').
mkRandomPosSpeed :: GenIO
                 -> Space
                 -> Coords Pos
                 -- ^ Precondition : is not colliding
                 -> IO PosSpeed
mkRandomPosSpeed gen space pos =
  fst
    . mirrorSpeedAndMoveToPrecollisionIfNeeded (`location` space)
    . PosSpeed pos <$> randomSpeed gen

data OverlapKind =
    NoOverlap
  | CanOverlap

data IndexedCoords = IndexedCoords {
    getIndex :: {-# UNPACK #-} !Int
  , getValue :: {-# UNPACK #-} !(Coords Pos)
}
instance Eq IndexedCoords where
  (IndexedCoords _ a) == (IndexedCoords _ b) = a == b
  {-# INLINABLE (==) #-}
instance Ord IndexedCoords where
  compare (IndexedCoords _ a) (IndexedCoords _ b) = compare a b
  {-# INLINABLE compare #-}

randomCCCoords :: GenIO -> Int -> ComponentIdx -> BigWorldTopology -> OverlapKind -> IO [Coords Pos]
randomCCCoords gen nPositions idxCC topo = \case
  CanOverlap -> map getValue . snd <$> genRandomCoords nPositions 0
  NoOverlap -> do
    let go (nextStartIndex, set) = do
          let remain = nPositions - Set.size set
          if remain == 0
            then
              return $ map getValue $ sortOn getIndex $ Set.toList set
            else
              go . fmap (Set.union set . Set.fromList) =<< genRandomCoords remain nextStartIndex
    go . fmap Set.fromList =<< genRandomCoords nPositions 0
 where
  genRandomCoords n startIndex = do
    l <- zipWith IndexedCoords
          [startIndex..]
          . map (getEltCoords topo idxCC) <$> replicateM n genRandomEltIndex
    return (startIndex + n,l)

  genRandomEltIndex = uniformR (0, pred $ getComponentSize topo idxCC) gen

randomSpeed :: GenIO -> IO (Coords Vel)
randomSpeed gen = do
    x <- rnd
    y <- rnd
    return $ Coords (Coord x) (Coord y)
  where
    rnd = uniformR (-1, 1) gen


-- | Creates a rectangular empty space of size specified in parameters.
mkEmptySpace :: Size -> BigWorld
mkEmptySpace s =
  BigWorld (mkSpaceFromMat s [[Air]]) $ mkEmptyBigWorldTopology s

mkFilledSpace :: Size -> BigWorld
mkFilledSpace s@(Size heightEmptySpace widthEmptySpace) =
  let w = fromIntegral widthEmptySpace
      h = fromIntegral heightEmptySpace
      l = replicate h $ replicate w Wall
  in BigWorld (mkSpaceFromMat s l) $ mkFilledBigWorldTopology s

-- | Creates a rectangular random space of size specified in parameters.
-- 'IO' is used for random numbers generation.
mkRandomlyFilledSpace :: WallDistribution
                      -> Size
                      -> ComponentCount
                      -> IO Bool
                      -- ^ Computation stops when it returns False
                      -> GenIO
                      -> IO (MkSpaceResult BigWorld, Maybe (Properties, Statistics))
mkRandomlyFilledSpace _ s 0 _ _ = return (Success $ mkFilledSpace s, Nothing)
mkRandomlyFilledSpace (WallDistribution blockSize wallAirRatio) s nComponents continue gen
  | blockSize <= 0 = fail $ "block size should be strictly positive : " ++ show blockSize
  | otherwise = mkSmallWorld gen property continue >>= \(res, stats) ->
  return
    (case res of
      NeedMoreTime -> NeedMoreTime
      Impossible bounds -> Impossible bounds
      Success small -> Success $ smallWorldToBigWorld s blockSize small
    , Just (property, stats))
 where
  property = mkProperties characteristics strategy
  characteristics = SWCharacteristics smallSz nComponents wallAirRatio
  smallSz = bigToSmall s blockSize
  strategy = bestStrategy characteristics

bestStrategy :: SmallWorldCharacteristics -> SmallWorldCreationStrategy
bestStrategy _ = -- TODO measure the best strategy for known cases, then interpolate.
  SWCreationStrategy Rotate Cyclic.Order1

smallWorldToBigWorld :: Size
                     -> Int
                     -> SmallWorld
                     -> BigWorld
smallWorldToBigWorld s blockSize small@(SmallWorld (SmallMatInfo _ smallWorldMat) _) =
  BigWorld (mkSpaceFromMat s $ map (map materialAndKeyToMaterial) innerMat) (smallToBig blockSize s small)
 where
  replicateElems = replicateElements blockSize
  innerMat = replicateElems $ map replicateElems $ Cyclic.toLists smallWorldMat

bigToSmall :: Size -> Int -> Size
bigToSmall (Size heightEmptySpace widthEmptySpace) blockSize =
  let nCols = quot widthEmptySpace $ fromIntegral blockSize
      nRows = quot heightEmptySpace $ fromIntegral blockSize
  in Size nRows nCols

{- | Generates a random world with the constraint that it should have
a single "Air" connected component. The function recurses and builds a new random world
until the constraint is met.
It might take "a long time" especially if worldsize is big and multFactor is small.
An interesting problem would be to compute the complexity of this function.
To do so we need to know the probability to have a unique connected component in
the random graph defined in the function.
-}
mkSmallWorld :: GenIO
             -> Properties
             -> IO Bool
             -- ^ Can continue?
             -> IO (MkSpaceResult SmallWorld, Statistics)
             -- ^ the "small world"
mkSmallWorld gen (Properties (SWCharacteristics sz nComponents' userWallProba) (SWCreationStrategy branch rotationOrder) eitherLowerBounds) continue
  | nComponents' == 0 = error "should be handled by caller"
  | otherwise = either
      (\err ->
        return (Impossible [err], zeroStats))
      (\lowerBounds -> withDuration (go lowerBounds) >>= \(dt, (r,stats)) ->
        return (case r of
                [] -> NeedMoreTime
                small:_ -> Success small
              , stats { durations = (durations stats) { totalDuration = dt }
                      }))
      eitherLowerBounds
 where

  go lowerBounds@(LowerBounds minAirCount minWallCount totalCount) =
    go' 0 zeroStats
   where
    go' i stats = continue >>= bool
      (return ([], stats))
        -- We use variations of the matrix to recycle random numbers, as random number generation is expensive.
      (fmap (either
          ((: []) . Left)
          -- stop at first success
          (takeWhilePlus isLeft . strategy branch))
        <$> withSampledDuration i (mkSmallMat gen wallProba sz lowerBounds) >>= \(duration, l) -> do
        let !newStats = addMkRandomMatDuration duration $ updateStats l stats
        case partitionEithers l of
          (_,[]) -> go' (i+1) newStats
          (_,successes) -> return (successes, finalizeStats i newStats))

    rndMatSamplingPeriod = 1000

    -- Measure the first sample, then wait rndMatSamplingPeriod iterations before measuring again.
    -- If you change this, please modify 'finalizeStats' accordingly.
    willMeasure i = (i :: Int) `rem` rndMatSamplingPeriod == 0

    withSampledDuration =
      bool (fmap ((,) zeroDuration)) withDuration . willMeasure

    finalizeStats i s =
      let d = durations s
          -- 'randomMatCreation' is the sum of /nMeasurements/ measurements done at the /beginning/ of every period.
          nMeasurements = 1 + quot i rndMatSamplingPeriod
          -- hence, assuming other executions have roughly the same duration, we can extrapolate:
          mult = fromIntegral (i+1) / fromIntegral nMeasurements
      in s { durations = d { randomMatCreation = mult .* randomMatCreation d } }

    wallProba = mapRange 0 1 minWallProba maxWallProba userWallProba
    minWallProba =     fromIntegral minWallCount / fromIntegral totalCount
    maxWallProba = 1 - fromIntegral minAirCount / fromIntegral totalCount

    tryRotationsIfAlmostMatches' = tryRotationsIfAlmostMatches rotationOrder matchTopology nComponents

    strategy Rotate m = tryRotationsIfAlmostMatches' m
    strategy InterleavePlusRotate m@(SmallMatInfo nAir mat) = r ++ i
     where
      r = tryRotationsIfAlmostMatches' m -- also checks m, hence we drop 1 in the next line
      i = map
          (matchTopology NCompsNotRequired nComponents . SmallMatInfo nAir)
          $ drop 1 $ Cyclic.produceUsefulInterleavedVariations mat
    strategy InterleaveTimesRotate m = withInterleaving tryRotationsIfAlmostMatches' m

  nComponents = min nComponents' maxNComp -- relax the constraint on number of components if the size is too small
  maxNComp = ComponentCount $ succ $ quot (pred $ area sz) 2 -- for checkerboard-like layout

  addMkRandomMatDuration dt s =
    let d = durations s
    in s { durations = d { randomMatCreation = randomMatCreation d |+| dt } }
  updateStats l s =
    let s' = List.foldl' addToStats s l -- 2.5% relative cost, measured using 'profileMkSmallWorld'
    in s' { countRandomMatrices = 1 + countRandomMatrices s}
  addToStats s elt =
   let s'' = case elt of
        Right (SmallWorld _ topo) ->
          let nComps = ComponentCount $ length $ getConnectedComponents topo
          in addNComp nComps s
        Left (NotEnough Air)  -> s { countNotEnoughAir    = 1 + countNotEnoughAir s }
        Left (NotEnough Wall) -> s { countNotEnoughWalls  = 1 + countNotEnoughWalls s }
        Left UnusedFronteers -> s { countUnusedFronteers = 1 + countUnusedFronteers s }
        Left (CC x nComps) -> addNComp nComps $ case x of
          ComponentCountMismatch ->
            s { countComponentCountMismatch = 1 + countComponentCountMismatch s }
          ComponentsSizesNotWellDistributed ->
            s { countComponentsSizesNotWellDistributed = 1 + countComponentsSizesNotWellDistributed s }
          SpaceNotUsedWellEnough ->
            s { countSpaceNotUsedWellEnough = 1 + countSpaceNotUsedWellEnough s }
          UnusedFronteers' ->
            s { countUnusedFronteers = 1 + countUnusedFronteers s }
   in s'' { countGeneratedMatrices = 1 + countGeneratedMatrices s'' }

  addNComp n s =
    s { countGeneratedGraphsByComponentCount =
      Map.alter (Just . (+1) . fromMaybe 0) n $ countGeneratedGraphsByComponentCount s }

withInterleaving :: (SmallMatInfo -> [a]) -> SmallMatInfo -> [a]
withInterleaving x (SmallMatInfo nAir mat) =
  concatMap (x . SmallMatInfo nAir) $ Cyclic.produceUsefulInterleavedVariations mat

type TopoMatch = Either SmallWorldRejection SmallWorld

getComponentCount :: TopoMatch -> Maybe ComponentCount
getComponentCount (Left (CC _ c)) = Just c
getComponentCount (Left _) = Nothing
getComponentCount (Right (SmallWorld _ topo)) = Just $ ComponentCount $ length $ getConnectedComponents topo

-- Indicates if there is a /real/ wall (i.e not an out of bounds position) in a given direction
data OrthoWall = OrthoWall {
    _dir :: !Direction
    -- ^ The direction in which to search
  , _p1 :: !(Maybe (Coords Pos))
    -- ^ 'Just' if a real wall is at distance 1 in the given 'Direction'.
}

matchTopology :: NCompsRequest
              -> ComponentCount
              -> SmallMatInfo
              -> TopoMatch
matchTopology nCompsReq nComponents r@(SmallMatInfo nAirKeys mat)
  | not airOnEveryFronteer = case nCompsReq of
      NCompsNotRequired -> Left UnusedFronteers
      NCompsRequiredWithPrecision _ -> Left $ CC UnusedFronteers' nComps
  | nComponents /= nComps = Left $ CC ComponentCountMismatch nComps
    -- from here on, comps is evaluated.
  | not wellDistributed   = Left $ CC ComponentsSizesNotWellDistributed nComps
    -- from here on, if the number of components is > 1, we compute the distances between components
  | not spaceIsWellUsed   = Left $ CC SpaceNotUsedWellEnough nComps
  | otherwise = Right $ SmallWorld r $ SmallWorldTopology comps (\i -> vtxToMatIdx UArray.! i)
 where
  nComps = ComponentCount $ length gcomps
  maxNCompsAsked = fromIntegral $ case nCompsReq of
    NCompsNotRequired -> nComponents + 1 -- + 1 so that the equality test makes sense
    NCompsRequiredWithPrecision x -> nComponents + 1 + x -- + 1 so that in tryRotationsIfAlmostMatches
                                                         -- it is possible to fail or succeed the distance test.

  -- mkGraph returns an undirected graph
  gcomps = Undirected.componentsN maxNCompsAsked $ mkGraph r

  vtxToMatIdx :: UArray.Array Int Int
  vtxToMatIdx = UArray.array (0,nAirKeys - 1) $ concatMap
    (\row -> let iRow = row * nCols in mapMaybe
      (\col -> let matIdx = iRow + col in case Cyclic.unsafeGetByIndex matIdx mat of
        MaterialAndKey (-1) -> Nothing
        MaterialAndKey k -> Just (k, matIdx :: Int))
      [0..nCols-1])
    [0..nRows-1]

  comps = map (ConnectedComponent . V.fromList . flatten) gcomps
  lengths = map countSmallCCElts comps
  wellDistributed = maximum lengths < 2 * minimum lengths

  airOnEveryFronteer =
    all
      (V.any (\case
        MaterialAndKey (-1) -> False
        MaterialAndKey _ -> True))
      fronteers
   where
    fronteers =
      [ Cyclic.getRow 0 mat
      , Cyclic.getRow (Cyclic.nrows mat - 1) mat
      , Cyclic.getCol 0 mat
      , Cyclic.getCol (Cyclic.ncols mat - 1) mat
      ]

  {- Returns True if the nearby graph (where an edge means 2 components are nearby)
  has only one connected component.

  Two components are nearby when they are either:

    * vertically separated by a single wall
    * horizontally separated by a single wall
    * diagonally adjacent

  Complexity : nVertex * lg nVertex.

  We traverse the space row by row to optimize memory access locality.

  * TODO Prune redundant lookups :
    * orthogonal lookups can be done in 2 directions (LEFT, Down) instead of 4
    * diagonal lookups can be done in 2 directions (Down LEFT, Down RIGHT) instead of 4
  -}
  spaceIsWellUsed
    | nComps <= 1 = True
    | otherwise = length compsOfCloseComps == 1
    where
      compsOfCloseComps =
        let (g,_,_) = Directed.graphFromSortedEdges $
              map
                (\(i,js) -> (i
                           , i
                           , Set.toList js))
                $ Map.toAscList closeComponentIndices
        in Directed.componentsN 2 g

      closeComponentIndices = List.foldl'
        (\edges' rowIdx ->
          -- walls must be detected by /matrix/ lookup.
          -- 'lookupComponent' is O(log V) and works for Air only.
          List.foldl' (\edges colIdx -> case Cyclic.unsafeGet rowIdx colIdx mat of
            MaterialAndKey (-1) -> edges
            MaterialAndKey k -> case neighbourComponents of
              [] -> edges
              l -> Map.insertWith Set.union component (Set.fromList l) edges
             where
              neighbourComponents =
                filter
                  (/= component)
                  $ catMaybes
                  $ lookupNearbyComponentsDiagonally ++ lookupNearbyComponentsOrthogonally

              component = lookupComponent k
              pos = Coords (fromIntegral rowIdx) (fromIntegral colIdx)

              lookupNearbyComponentsDiagonally =
                go $ take 5 $ cycle cyclicOrthoWalls
               where
                go []  = []
                go [_] = []
                go (OrthoWall d1 (Just _) : rest@(OrthoWall _ (Just w2) : _)) =
                  let (Coords (Coord diagRow) (Coord diagCol)) = translateInDir d1 w2
                      -- Note that (translateInDir d1 w2) is equivalent to (translateInDir d2 w1).
                      -- Also, by construction, diag is withinBounds because both OrthoWalls are.
                      -- Hence we skip the 'withinBounds' test.
                  in (case getMaterialAndKey diagRow diagCol of
                      MaterialAndKey (-1) -> Nothing
                      (MaterialAndKey diagK) -> Just $ lookupComponent diagK) : go rest
                go (_:rest@(_:_)) = go rest

              lookupNearbyComponentsOrthogonally = map lookupOrthogonally cyclicOrthoWalls

              lookupOrthogonally (OrthoWall _ Nothing) = Nothing
              lookupOrthogonally (OrthoWall dir (Just wall1Pos))
               | withinBounds r2 c2 = case getMaterialAndKey r2 c2 of
                    MaterialAndKey (-1) -> Nothing
                    MaterialAndKey afterWallK -> Just $ lookupComponent afterWallK
               | otherwise = Nothing
               where
                (Coords (Coord r2) (Coord c2)) = translateInDir dir wall1Pos

              cyclicDirs = [LEFT,Up,RIGHT,Down]
              cyclicOrthoWalls = map orthoWallInDir cyclicDirs

              orthoWallInDir :: Direction -> OrthoWall
              orthoWallInDir d =
                let p@(Coords (Coord row) (Coord col)) = translateInDir d pos
                in OrthoWall d $
                  bool
                    Nothing
                    (case getMaterialAndKey row col of
                       MaterialAndKey (-1) -> Just p
                       MaterialAndKey _ -> Nothing)
                    $ withinBounds row col

              withinBounds row col =
                row >= 0 && col >= 0 && row < nRows && col < nCols

              getMaterialAndKey row col = Cyclic.unsafeGet row col mat)
            edges'
            [0..nCols-1])
        -- initialize with every component index
        (Map.fromDistinctAscList $
          zip
            [0..fromIntegral $ nComps-1]
            $ repeat Set.empty :: Map ComponentIdx (Set ComponentIdx))
        [0..nRows-1]

      -- Int is an Air key. This function errors if the key is out of bounds.
      lookupComponent :: Int -> ComponentIdx
      lookupComponent = (V.!) keyToComponent

  keyToComponent :: V.Vector ComponentIdx -- Indexed by Air keys
  keyToComponent = V.create $ do
    v <- VM.new nAirKeys
    mapM_
      (\(compIdx, ConnectedComponent vertices) -> V.mapM_ (flip (VM.write v) compIdx) vertices)
      $ zip [0 :: ComponentIdx ..] comps
    return v

  nRows = Cyclic.nrows mat
  nCols = Cyclic.ncols mat

thresholdDiffComponentCount :: ComponentCount
thresholdDiffComponentCount = 5

tryRotationsIfAlmostMatches :: Cyclic.RotationOrder
                            -> (NCompsRequest -> ComponentCount -> SmallMatInfo -> TopoMatch)
                            -> ComponentCount
                            -> SmallMatInfo
                            -> [TopoMatch]
tryRotationsIfAlmostMatches order matchTopo n zeroRotation@(SmallMatInfo nAir matZeroRotation) =

  zeroRotationRes : tryRotation

 where

  tryRotation = case zeroRotationRes of
    Right _ -> []
    Left (CC _ nComps) -> try nComps
    Left _ -> []

  try nComps
   | -- TODO Maybe the bound should be randomized? or it should be min w h of the world?
     abs (n - nComps) <= thresholdDiffComponentCount =
     -- we are already close to the target number, so
     -- there is a good probability that rotating will trigger the component match.
     map (matchTopo NCompsNotRequired n . SmallMatInfo nAir) $ Cyclic.produceRotations order matZeroRotation
   | otherwise = []

  zeroRotationRes = matchTopo
    (case order of
      Cyclic.Order0 -> NCompsNotRequired
      _ -> NCompsRequiredWithPrecision thresholdDiffComponentCount)
    n zeroRotation

mkSmallMat :: GenIO
           -> Float
           -- ^ Probability to generate a wall
           -> Size
           -- ^ Size of the matrix
           -> LowerBounds
           -> IO (Either SmallWorldRejection SmallMatInfo)
           -- ^ in Right, the Air keys are ascending, consecutive, and start at 0
mkSmallMat gen wallProba (Size nRows nCols) (LowerBounds minAirCount minWallCount countBlocks)
  | countBlocks /= fromIntegral nRows * fromIntegral nCols = error "logic"
  | minAirCount + minWallCount > countBlocks = error "logic" -- this is checked when creating 'LowerBounds'
  | otherwise = go <$> replicateM countBlocks (uniform gen)
 where
  go floats
   | countAirBlocks < minAirCount = Left $ NotEnough Air
   | countWallBlocks < minWallCount = Left $ NotEnough Wall
   | otherwise = Right $ SmallMatInfo countAirBlocks $ Cyclic.fromList (fromIntegral nRows) (fromIntegral nCols) wallsAndDescendingAirKeys
   where
    (countAirBlocks, wallsAndDescendingAirKeys) = mkWallsAndDescendingAirKeys wallProba floats

    countWallBlocks = countBlocks - countAirBlocks


mkWallsAndDescendingAirKeys :: Float -- probability of a wall
                            -> [Float] -- produced by randBiased
                            -> (Int, [MaterialAndKey])
mkWallsAndDescendingAirKeys wallProba =
  go 0 []
 where
  -- the Air elements for a list of consecutive /descending/ keys
  go k l (p:ps)
   -- Air
   | p > wallProba = go (k+1) (MaterialAndKey k   :l) ps
   -- Wall
   | otherwise     = go k     (MaterialAndKey (-1):l) ps
  go k l [] = (k, l)

-- | Used in 'LowerBounds' test.
unsafeMkSmallMat :: GenIO
                 -> Float
                 -- ^ Probability to generate a wall
                 -> Size
                 -- ^ Size of the matrix
                 -> IO SmallMatInfo
unsafeMkSmallMat gen wallAirRatio s@(Size nRows nCols) = do
  (nAir, l) <- mkWallsAndDescendingAirKeys wallAirRatio <$> replicateM (area s) (uniform gen)
  return $ SmallMatInfo nAir $ Cyclic.fromList (fromIntegral nRows) (fromIntegral nCols) l


{-# INLINE countSmallCCElts #-}
countSmallCCElts :: ConnectedComponent -> Int
countSmallCCElts (ConnectedComponent v) = V.length v

{-# INLINE countBigCCElts #-}
countBigCCElts :: Int -> ConnectedComponent -> Int
countBigCCElts blockSize c =
  blockSize * blockSize * countSmallCCElts c -- doesn't include extensions.

getSmallMatIndex :: Int -> (Undirected.Vertex -> Int) -> ConnectedComponent -> Int
getSmallMatIndex smallIndex resolver c@(ConnectedComponent v)
  | smallIndex < 0 || smallIndex >= countSmallCCElts c = error $ "index out of bounds:" ++ show smallIndex
  | otherwise = resolver $ v V.! smallIndex

mkEmptyBigWorldTopology :: Size -> BigWorldTopology
mkEmptyBigWorldTopology s@(Size nRows _) =
  BigWorldTopology 1 sz coords
 where
  sz (ComponentIdx 0) = area s
  sz i = error $ "index out of range " ++ show i
  coords i eltIdx
    | eltIdx < 0 || eltIdx >= sz i = error $ "out of range " ++ show (i, eltIdx)
    | otherwise = Coords (fromIntegral row) (fromIntegral col)
        where (col, row) = eltIdx `quotRem` fromIntegral nRows

mkFilledBigWorldTopology :: Size -> BigWorldTopology
mkFilledBigWorldTopology _ =
  BigWorldTopology 0 sz coords
 where
  sz i = error $ "index out of range " ++ show i
  coords i eltIdx = error $ "out of range " ++ show (i, eltIdx)

smallToBig :: Int -> Size -> SmallWorld -> BigWorldTopology
smallToBig blockSize bigSize s@(SmallWorld _ (SmallWorldTopology ccs _)) =
  BigWorldTopology l (countBigCCElts blockSize . safeGetCC) coords
 where
  l = length ccs
  coords i eltIdx =
    getBigCoords eltIdx blockSize bigSize s $ safeGetCC i
  safeGetCC (ComponentIdx i)
   | i < 0 || i >= l = error $ "index out of bounds:" ++ show i
   | otherwise = ccs !! i

getBigCoords :: Int -> Int -> Size -> SmallWorld -> ConnectedComponent -> Coords Pos
getBigCoords bigIndex blockSize (Size nBigRows nBigCols) (SmallWorld (SmallMatInfo _ smallMat) (SmallWorldTopology _ resolver)) component =
  let modulo = blockSize * blockSize
      (smallIndex, remain) = bigIndex `quotRem` modulo
      (remainRow, remainCol) = remain `quotRem` blockSize
      smallCoords = Coords (fromIntegral r) (fromIntegral c)
      (r,c) = quotRem (getSmallMatIndex smallIndex resolver component) nSmallCols
      nSmallRows = Cyclic.nrows smallMat
      nSmallCols = Cyclic.ncols smallMat
      (rowStart, _) = extend'' (fromIntegral nBigRows) $ fromIntegral nSmallRows * blockSize
      (colStart, _) = extend'' (fromIntegral nBigCols) $ fromIntegral nSmallCols * blockSize
      bigUpperLeft = Coords (fromIntegral rowStart) (fromIntegral colStart)
  in translate bigUpperLeft $
       translate (Coords (fromIntegral remainRow) (fromIntegral remainCol)) $
       multiply blockSize smallCoords

-- | Creates an undirected graph
mkGraph :: SmallMatInfo -> Undirected.Graph
mkGraph (SmallMatInfo nAirKeys mat) =
  BV.create $ do
    v <- BVM.new nAirKeys
    forM_ [0..nRows-1] (\row -> do
      let iRow = row * nCols
      forM_ [0..nCols-1] (\col -> do
        let matIdx = iRow + col
        case Cyclic.unsafeGetByIndex matIdx mat of
          MaterialAndKey (-1) -> return ()
          MaterialAndKey k -> BVM.write v k $ neighbourAirKeys matIdx row col))
    return v

 where

  nRows = Cyclic.nrows mat
  nCols = Cyclic.ncols mat

  neighbourAirKeys :: Int -> Int -> Int -> [Int]
  neighbourAirKeys matIdx row col =
    mapMaybe (\neighbourMatIdx -> case Cyclic.unsafeGetByIndex neighbourMatIdx mat of
      MaterialAndKey (-1) -> Nothing
      MaterialAndKey k -> Just k)
    $ catMaybes
    -- it is faster to write all directions at once than just 2, and wait for
    -- other nodes to add other directions.
    [ bool (Just $ matIdx - nCols) Nothing $ row == 0       -- Up
    , bool (Just $ matIdx - 1)     Nothing $ col == 0       -- LEFT
    , bool (Just $ matIdx + 1)     Nothing $ col == nCols-1 -- RIGHT
    , bool (Just $ matIdx + nCols) Nothing $ row == nRows-1 -- Up
    ]

mkSpaceFromMat :: Size -> [[Material]] -> Space
mkSpaceFromMat s matMaybeSmaller =
  Space $ Unboxed.fromLists $ extend s matMaybeSmaller

extend :: Size -> [[a]] -> [[a]]
extend (Size rs cs) mat =
  extend' (fromIntegral rs) $ map (extend' $ fromIntegral cs) mat

extend' :: Int -> [a] -> [a]
extend' _ [] = error "extend empty list not supported"
extend' sz l@(_:_) =
  replicate addsLeft (head l) ++
  l ++
  replicate addsRight (last l)
 where
  (addsLeft, addsRight) = extend'' sz $ length l

extend'' :: Int -> Int -> (Int,Int)
extend'' final initial =
  let addsTotal = final - assert (initial <= final) initial
      addsLeft = quot addsTotal 2
      addsRight = addsTotal - addsLeft
  in (addsLeft, addsRight)

-- | <https://hackage.haskell.org/package/matrix-0.3.5.0/docs/Data-Matrix.html#v:getElem Indices start at 1>:
-- @Coord 0 0@ corresponds to indexes 1 1 in matrix
getMaterial :: Coords Pos -> Space -> Material
getMaterial coords@(Coords r c) space
  | r < 0 || c < 0 = Wall
  | r >= fromIntegral rs || c >= fromIntegral cs = Wall
  | otherwise = unsafeGetMaterial coords space
  where
    (Size rs cs) = getSize space

-- | If 'Coords' is inside 'Space', returns 0. Else returns
-- the manhattan distance to the space border.
distanceToSpace :: Coords Pos -> Space -> Int
distanceToSpace (Coords r c) space =
  dist r (rs-1) + dist c (cs-1)
 where
  (Size rs cs) = getSize space
  dist x b
   | x < 0 = fromIntegral $ - x
   | x > fromIntegral b = fromIntegral x - fromIntegral b
   | otherwise = 0

materialToLocation :: Material -> Location
materialToLocation m = case m of
  Wall -> OutsideWorld
  Air  -> InsideWorld

-- | Considers that outside 'Space', everything is 'OutsideWorld'
location :: Coords Pos -> Space -> Location
location c s =
  materialToLocation $ getMaterial c s
