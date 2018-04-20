{-# OPTIONS_HADDOCK hide #-}
{-# OPTIONS_GHC -O2 #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

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
    , bigToSmall
    -- for tests
    , matchAndVariate
    , mkSmallWorld
    , fillSmallVector
    , matchTopology
    , TopoMatch
    ) where

import           Imj.Prelude
import           Prelude(putStrLn)
import qualified Prelude as Unsafe(head,last)
import           Control.Monad.ST(runST)
import           Control.Concurrent(MVar, takeMVar, tryPutMVar, newEmptyMVar)
import           Control.Concurrent.Async(withAsync)
import           Data.Primitive.ByteArray
import qualified Data.Array.Unboxed as UArray(Array, array, (!))
import           Data.Bits(shiftR, shiftL, (.|.))
import           Data.Either(isLeft)
import           Data.List(unwords, length, sortOn, replicate, take, foldl')
import           Data.List.NonEmpty(NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.List as List
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map
import           Data.Set(Set)
import qualified Data.Set as Set(size, empty, fromList, toList, union)
import           Data.Tree(flatten, foldTree)
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as MS
import qualified Data.Vector.Unboxed.Mutable as MV
import qualified Data.Vector.Unboxed as V
import           Data.Word(Word8)
import           GHC.Word(Word64, Word16)
import           System.Random.MWC(GenIO, uniformR, foldMUniforms)

import           Imj.Data.AlmostFloat
import qualified Imj.Data.Graph as Directed(graphFromSortedEdges, componentsN)
import qualified Imj.Data.UndirectedGraph as Undirected
import qualified Imj.Data.Matrix.Unboxed as Unboxed
import qualified Imj.Data.Matrix.Cyclic as Cyclic
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Game.Hamazed.World.Space.Strategies
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
                      -> NonEmpty GenIO
                      -> IO (MkSpaceResult BigWorld, Maybe (Properties, Statistics))
mkRandomlyFilledSpace _ s 0 _ _ = return (Success $ mkFilledSpace s, Nothing)
mkRandomlyFilledSpace (WallDistribution blockSize wallAirRatio) s nComponents continue gens
  | blockSize <= 0 = fail $ "block size should be strictly positive : " ++ show blockSize
  | otherwise = do
      (strategy,mayEstimation) <- either
        (\(ClosestOptimalStrategy mayCloseWorld mayDuration strategy) -> do
            maybe
              (putStrLn "No close world was found.")
              (\close -> do
                    let dist = almost $ smallWorldCharacteristicsDistance close characteristics
                    putStrLn $ unwords
                      [ "Found\n"
                      , prettyShowSWCharacteristics close
                      , "\nat distance"
                      , show dist
                      , "of\n"
                      , prettyShowSWCharacteristics characteristics]
                    putStrLn $ "Close strategy:" ++ show strategy)
              mayCloseWorld
            return (strategy,mayDuration))
        (\(OptimalStrategy exactMatch duration) -> do
            putStrLn $ unwords
              [ "Found an exact match for"
              , prettyShowSWCharacteristics characteristics]
            putStrLn $ "Strategy:" ++ show exactMatch
            return (exactMatch, Just duration))
        $ closestOptimalStrategy characteristics
      putStrLn $
        "Estimated duration:" ++
        maybe "no estimation" showTime mayEstimation
      let property = mkProperties characteristics $ fmap (toVariants smallSz) strategy
      mkSmallWorld gens property continue >>= \(res, stats) ->
        return
          (case res of
            NeedMoreTime -> NeedMoreTime
            Impossible bounds -> Impossible bounds
            Success small -> Success $ smallWorldToBigWorld s blockSize small
          , Just (property, stats))
 where
  smallSz = bigToSmall s blockSize
  characteristics = SWCharacteristics smallSz nComponents wallAirRatio

smallWorldToBigWorld :: Size
                     -> Int
                     -> SmallWorld
                     -> BigWorld
smallWorldToBigWorld s blockSize small@(SmallWorld (SmallMatInfo _ smallWorldMat) _) =
  BigWorld (mkSpaceFromMat s $ map (map materialAndKeyToMaterial) innerMat) (smallToBig blockSize s small)
 where
  replicateElems :: [a] -> [a]
  replicateElems = replicateElements blockSize
  innerMat = replicateElems $ map replicateElems $ Cyclic.toLists smallWorldMat

bigToSmall :: Size -> Int -> Size
bigToSmall (Size heightEmptySpace widthEmptySpace) blockSize =
  let nCols = quot widthEmptySpace $ fromIntegral blockSize
      nRows = quot heightEmptySpace $ fromIntegral blockSize
  in Size nRows nCols


data MatrixPipeline = MatrixPipeline !MatrixSource !MatrixTransformer

newtype MatrixSource = MatrixSource (MS.IOVector MaterialAndKey -> GenIO -> IO SmallMatInfo)

newtype MatrixTransformer = MatrixTransformer (Statistics -> ByteArray -> SmallMatInfo -> BestRandomMatrixVariation)

data BestRandomMatrixVariation = BRMV {
  _topology :: !TopoMatch
  -- ^ Best found sofar
  , _stats :: !Statistics
  -- ^ Records stats about the search
}

type TopoMatch = Either SmallWorldRejection SmallWorld


-- We need n locations to store random matrices.
-- We need to wait when more than m locations are used.
mkMatrixPipeline :: ComponentCount
                 -> AlmostFloat
                 -- ^ Probability to generate a wall
                 -> Size
                 -- ^ Size of the matrix
                 -> LowerBounds
                 -> Maybe MatrixVariants
                 -> IO MatrixPipeline
mkMatrixPipeline nComponents' wallProba (Size (Length nRows) (Length nCols)) lb variants =
  return $ MatrixPipeline (MatrixSource produce) $ MatrixTransformer consume
 where
  produce v gen =
    fillSmallVector gen wallProba v >>= \nAir ->
      SmallMatInfo (fromIntegral nAir) . Cyclic.fromVector nRows nCols <$> S.unsafeFreeze v

  consume s v mat =
    foldStats (s{countRandomMatrices = 1 + countRandomMatrices s}) $ either
          ((:|[]) . Left)
          (takeWhilePlus isLeft . matchAndVariate nComponents variants v) $ checkRandomMatrix lb mat

  !nComponents = -- relax the constraint on number of components if the size is too small
    min
      nComponents'
      $ ComponentCount $ 1 + quot (nBlocks - 1) 2 -- for checkerboard-like layout

  nBlocks = nRows * nCols

runPipeline :: Int -> NonEmpty GenIO -> IO Bool -> MatrixPipeline -> IO (Maybe SmallWorld, Statistics)
runPipeline nBlocks generators continue (MatrixPipeline (MatrixSource produce) (MatrixTransformer consume)) = do
  resVar <- newEmptyMVar :: IO (MVar (Maybe SmallWorld, Statistics))
  run resVar
 where
  run resM =
    run' (NE.toList generators)
   where
    run' [] =
      takeMVar resM -- the first will win

    run' (gen:gens) =
      withAsync (shortcut gen) $ \_ ->
        -- print =<< threadCapability (asyncThreadId a)
        run' gens

    -- Note that running producer and consummer in separate threads, and forwarding the results
    -- through an MVar is slower than calling them sequentially, like here.
    shortcut gen = do
      -- we align to 64 byte and allocate a multiple of 64 bytes to avoid false sharing
      -- (assuming a cache line size of 64 bytes)
      graphArray <- newAlignedPinnedByteArray (ceilToMultiple 64 $ nBlocks * 8) 64 >>= unsafeFreezeByteArray
      -- TODO Take a slice of a bigger vector to avoid false sharing here, too.
      matV <- MS.unsafeNew nBlocks
      go' matV graphArray
     where
      go' v ba =
        go zeroStats
       where
        go s = continue >>= \case
          False ->
            void $ tryPutMVar resM (Nothing, s)
          True ->
            consume s ba <$> produce v gen >>= \(BRMV m s') ->
                either (const $ go s') (void . tryPutMVar resM . flip (,) s' . Just) m

checkRandomMatrix :: LowerBounds -> SmallMatInfo -> Either SmallWorldRejection SmallMatInfo
checkRandomMatrix (LowerBounds minAirCount minWallCount countBlocks) m@(SmallMatInfo countAirBlocks _)
   | countAirBlocks < minAirCount = Left $ NotEnough Air
   | countWallBlocks < minWallCount = Left $ NotEnough Wall
   | otherwise = Right m
   where
    !countWallBlocks = countBlocks - countAirBlocks

mkSmallWorld :: NonEmpty GenIO
             -> Properties
             -> IO Bool
             -- ^ Can continue?
             -> IO (MkSpaceResult SmallWorld, Statistics)
             -- ^ the "small world"
mkSmallWorld gens (Properties (SWCharacteristics sz nComponents' userWallProba) variants eitherLowerBounds) continue
  | nComponents' == 0 = error "should be handled by caller"
  | otherwise = either
      (\err ->
        return (Impossible [err], zeroStats))
      (\lowerBounds ->
        withDuration (go lowerBounds) >>= \(dt, (r,stats)) ->
          return (maybe NeedMoreTime Success r
                , stats { durations = (durations stats) { totalDuration = dt }
                        }))
      eitherLowerBounds
 where
  go lowerBounds@(LowerBounds minAirCount minWallCount totalCount) =
    mkMatrixPipeline nComponents wallProba sz lowerBounds variants >>= runPipeline (area sz) gens continue
   where
    wallProba = fromMaybe (error "logic") $ mapRange 0 1 minWallProba maxWallProba userWallProba
    minWallProba =     fromIntegral minWallCount / fromIntegral totalCount
    maxWallProba = 1 - fromIntegral minAirCount / fromIntegral totalCount

  !nComponents = -- relax the constraint on number of components if the size is too small
    min
      nComponents'
      $ ComponentCount $ 1 + quot (area sz - 1) 2 -- for checkerboard-like layout


-- gathers 'Statistics' of 'TopoMatch'es and returns the last 'TopoMatch'.
foldStats :: Statistics -> NonEmpty TopoMatch -> BestRandomMatrixVariation
foldStats stats (x:|xs) =
  List.foldl' (\(BRMV _ s) v -> BRMV v $ addToStats v s) (BRMV x $ addToStats x stats) xs

addToStats' :: SmallWorldRejection -> Statistics -> Statistics
addToStats' (NotEnough Air) s = s { countNotEnoughWalls  = 1 + countNotEnoughWalls s }
addToStats' (NotEnough Wall) s = s { countNotEnoughWalls  = 1 + countNotEnoughWalls s }
addToStats' UnusedFronteers s = s { countUnusedFronteers = 1 + countUnusedFronteers s }
addToStats' (CC x nComps) s = addNComp nComps $ case x of
  ComponentCountMismatch ->
    s { countComponentCountMismatch = 1 + countComponentCountMismatch s }
  ComponentsSizesNotWellDistributed ->
    s { countComponentsSizesNotWellDistributed = 1 + countComponentsSizesNotWellDistributed s }
  SpaceNotUsedWellEnough ->
    s { countSpaceNotUsedWellEnough = 1 + countSpaceNotUsedWellEnough s }
  UnusedFronteers' ->
    s { countUnusedFronteers = 1 + countUnusedFronteers s }

addToStats'' :: SmallWorld -> Statistics -> Statistics
addToStats'' (SmallWorld _ topo) s =
  let nComps = ComponentCount $ length $ getConnectedComponents topo
  in addNComp nComps s

addToStats :: TopoMatch -> Statistics -> Statistics
addToStats elt s =
 let s'' = case elt of
      Right r -> addToStats'' r s
      Left l  -> addToStats' l s
 in s'' { countGeneratedMatrices = 1 + countGeneratedMatrices s'' }

addNComp :: ComponentCount -> Statistics -> Statistics
addNComp n s =
  s { countGeneratedGraphsByComponentCount =
    Map.alter (Just . (+1) . fromMaybe 0) n $ countGeneratedGraphsByComponentCount s }

matchAndVariate :: ComponentCount
                -> Maybe MatrixVariants
                -> ByteArray
                -> SmallMatInfo
                -> NonEmpty TopoMatch
matchAndVariate nComponents curB v info =
  rootRes :| variationResults
 where
  variationResults = case info of
    (SmallMatInfo nAir m) ->
      maybe []
        (\(Variants variations nextB) ->
          let authorizeVariation (Modulate _ _) = True
              authorizeVariation (Interleave _ _) = True
              authorizeVariation (Rotate (RotationDetail _ margin)) =
                case rootRes of
                  Left (CC _ nComps) -> abs (nComponents - nComps) <= margin
                  _ -> False

              produceVariations (Modulate from to) =
                map (`Cyclic.modulate` m) [from..to]

              produceVariations (Interleave r c) =
                drop 1 $ Cyclic.produceUsefulInterleavedVariations r c m

              produceVariations (Rotate (RotationDetail order _)) =
                Cyclic.produceRotations order m
          -- TODO try conduit or pipes or https://www.twanvl.nl/blog/haskell/streaming-vector in
          --    matchAndVariate
          --    random matrix creation
          --    matrix production.
          -- See if it is faster / if the code is more modular.
          -- https://stackoverflow.com/questions/14259229/streaming-recursive-descent-of-a-directory-in-haskell
          -- https://gist.github.com/FranklinChen/133cb61af931a08bbe20
          in List.concatMap -- TODO benchmark which is faster : consuming depth first or breadth first
              (NE.toList . matchAndVariate nComponents nextB v . SmallMatInfo nAir)
              $ concatMap produceVariations
              $ NE.filter authorizeVariation variations)
        curB

  deepMargin = requiredNComponentsMargin curB
  rootRes = matchTopology deepMargin nComponents info v


requiredNComponentsMargin :: Maybe MatrixVariants -> NCompsRequest
requiredNComponentsMargin Nothing = NCompsNotRequired
requiredNComponentsMargin (Just (Variants variations nextB)) =
  max' thisValue recursiveValue
 where
  thisValue = List.foldl' max' NCompsNotRequired $ NE.map requiredNComponentsMarginForVariation variations
  recursiveValue = requiredNComponentsMargin nextB

  max' NCompsNotRequired a = a
  max' a NCompsNotRequired = a
  max' (NCompsRequiredWithMargin a) (NCompsRequiredWithMargin b) = NCompsRequiredWithMargin $ max a b

requiredNComponentsMarginForVariation :: Variation -> NCompsRequest
requiredNComponentsMarginForVariation (Modulate _ _)   = NCompsNotRequired
requiredNComponentsMarginForVariation (Interleave _ _) = NCompsNotRequired
requiredNComponentsMarginForVariation (Rotate (RotationDetail _ margin)) = NCompsRequiredWithMargin margin

-- | Takes elements matching a condition, and the element thereafter.
takeWhilePlus :: (a -> Bool) -> NonEmpty a -> NonEmpty a
takeWhilePlus p (e:|es) =
  e :| bool [] (go es) (p e)
 where
  go (x:xs) = x : bool [] (go xs) (p x)
  go [] = []

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
              -> ByteArray
              -> TopoMatch
matchTopology !nCompsReq nComponents r@(SmallMatInfo nAirKeys mat) ba
  | not airOnEveryFronteer = case nCompsReq of
      NCompsNotRequired -> Left UnusedFronteers
      NCompsRequiredWithMargin _ -> Left $ CC UnusedFronteers' nComps
  | nComponents /= nComps = Left $ CC ComponentCountMismatch nComps
    -- from here on, comps is evaluated.
  | not (wellDistributed lengthsMinMax) = Left $ CC ComponentsSizesNotWellDistributed nComps
    -- from here on, if the number of components is > 1, we compute the distances between components
  | not spaceIsWellUsed   = Left $ CC SpaceNotUsedWellEnough nComps
  | otherwise = Right $ SmallWorld r $ SmallWorldTopology comps
      (\i ->
        let vtxToMatIdx :: UArray.Array Int Int
            vtxToMatIdx = UArray.array (0,nAirKeys - 1) $ mapMaybe
              (\matIdx -> isAir Nothing (\k -> Just (fromIntegral k, matIdx)) $
                Cyclic.unsafeGetByIndex matIdx mat)
              [0..Cyclic.nelems mat-1]
        in fromIntegral $ vtxToMatIdx UArray.! fromIntegral i)
 where
  !airOnEveryFronteer =
    all
      (S.any (isAir False (const True)))
      fronteers
   where
    fronteers =
      [ Cyclic.getRow 0 mat
      , Cyclic.getRow (nRows - 1) mat
      , Cyclic.getCol 0 mat
      , Cyclic.getCol (nCols - 1) mat
      ]

  !nRows = Cyclic.nrows mat
  !nCols = Cyclic.ncols mat

  maxNCompsAsked = fromIntegral $ case nCompsReq of
    NCompsNotRequired -> nComponents + 1 -- + 1 so that the equality test makes sense
    NCompsRequiredWithMargin x -> nComponents + 1 + x -- + 1 so that in tryRotationsIfAlmostMatches
                                                      -- it is possible to fail or succeed the distance test.

  -- To optimize performances, we use the fact that mono-components are easily detected
  -- while building the graph : they don't have any neighbour.
  -- If the lower bound of the number of component is already bigger than the max number needed, we are done.
  -- Else, we need to detect components using Undirected.componentsN on the graph.
  nComps
   | nMinComps >= maxNCompsAsked = nMinComps
   | otherwise = ComponentCount (length allComps)

  allComps = Undirected.componentsN
    (fromIntegral maxNCompsAsked)
    $ fromMaybe (error "logic") graph

  -- returns a minimum bound on the number of components.
  -- if the lowerbound of the number of cc is >= to the limit,
  --   a Nothing graph is returned.
  --   else a Just undirected graph is returned.
  (graph, nMinComps) = mkGraphWithStrictlyLess maxNCompsAsked r ba

  comps = map (ConnectedComponent . V.fromList . flatten) allComps

  lengthsMinMax = foldl'
    (\mm comp ->
      let len = foldTree (\_ l -> 1 + foldl' (+) 0 l) comp
      in addValue len mm)
    mkEmptyMinMax
    allComps

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
          List.foldl' (`folder` rowIdx) edges' [0..nCols-1])
        -- initialize with every component index
        (Map.fromDistinctAscList $
          zip
            [0..fromIntegral $ nComps-1]
            $ repeat Set.empty :: Map ComponentIdx (Set ComponentIdx))
        [0..nRows-1]

      folder edges rowIdx colIdx = isAir edges onAir $ Cyclic.unsafeGet rowIdx colIdx mat
       where
        onAir k = case neighbourComponents of
          [] -> edges
          l -> Map.insertWith Set.union component (Set.fromList l) edges
         where
          neighbourComponents =
            filter
              (/= component)
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
              in isAir [] ((:[]) . lookupComponent) (getMaterialAndKey diagRow diagCol) ++
                  go rest
            go (_:rest@(_:_)) = go rest

          lookupNearbyComponentsOrthogonally = concatMap lookupOrthogonally cyclicOrthoWalls

          lookupOrthogonally (OrthoWall _ Nothing) = []
          lookupOrthogonally (OrthoWall dir (Just wall1Pos))
           | withinBounds r2 c2 = isAir [] ((:[]) . lookupComponent) $ getMaterialAndKey r2 c2
           | otherwise = []
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
                (isAir (Just p) (const Nothing) $ getMaterialAndKey row col)
                $ withinBounds row col

          withinBounds row col =
            row >= 0 && col >= 0 && row < nRows && col < nCols

          getMaterialAndKey row col = Cyclic.unsafeGet row col mat

      -- Int is an Air key. This function errors if the key is out of bounds.
      lookupComponent :: Word16 -> ComponentIdx
      lookupComponent i = (V.!) keyToComponent $ fromIntegral i

      keyToComponent :: V.Vector ComponentIdx -- Indexed by Air keys
      keyToComponent =
        runST $ do
          v <- MV.unsafeNew $ fromIntegral nAirKeys
          mapM_
            (\(compIdx, ConnectedComponent vertices) -> V.mapM_ (flip (MV.unsafeWrite v) compIdx . fromIntegral) vertices)
            $ zip [0 :: ComponentIdx ..] comps
          V.unsafeFreeze v

-- | Where min > max means empty
data MinMax = MinMax {
    _min :: {-# UNPACK #-} !Int
  , _max :: {-# UNPACK #-} !Int
}

mkEmptyMinMax :: MinMax
mkEmptyMinMax = MinMax 1 (-1)

addValue :: Int -> MinMax -> MinMax
addValue !i m@(MinMax mi ma)
  | mi > ma = MinMax i i
  | i < mi = MinMax i ma
  | i > ma = MinMax mi i
  | otherwise = m

wellDistributed :: MinMax -> Bool
wellDistributed (MinMax mi ma)
  | mi >= ma = True
  | otherwise = ma < 2 * mi

data AccumSource = AS {
    countAirKeys :: {-# UNPACK #-} !Word16
  , _index :: {-# UNPACK #-} !Int
}

fillSmallVector :: GenIO
                -> AlmostFloat
                -- ^ Probability to generate a wall
                -> MS.IOVector MaterialAndKey
                -- ^ Use this memory
                -> IO Word16
                -- ^ The count of air keys
fillSmallVector gen wallProba v = do
  let countBlocks = MS.length v
      !limit = mapNormalizedToDiscrete wallProba maxBound :: Word8

      source8' :: Int -> (Int -> Word16 -> Word8 -> IO Word16) -> IO Word16
      source8' n f =
        countAirKeys <$> foldMUniforms nWord32 accF (AS 0 0) gen
       where
        nWord32 = 1 + quot (n-1) 4

        accF (AS s i) w32 = do
          let w1 = fromIntegral w32 :: Word8
              w2 = fromIntegral (w32 `shiftR` 8) :: Word8
              w3 = fromIntegral (w32 `shiftR` 16) :: Word8
              w4 = fromIntegral (w32 `shiftR` 24) :: Word8

          f    i    s  w4 >>= \s1 ->
            f (i+1) s1 w3 >>= \s2 ->
            f (i+2) s2 w2 >>= \s3 ->
            f (i+3) s3 w1 >>= \s4 -> return (AS s4 $ i+4)

      buildVector i nAir word
        | i >= countBlocks = return nAir
        | otherwise = do
            MS.unsafeWrite v i $ MaterialAndKey $ bool 0xFFFF nAir air
            return $ bool nAir (nAir+1) air
        where
          air = word > limit

  source8' countBlocks buildVector

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

  coords !i !eltIdx
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
  !l = length ccs

  coords i eltIdx =
    getBigCoords eltIdx blockSize bigSize s $ safeGetCC i

  safeGetCC (ComponentIdx i)
   | i < 0 || i >= l = error $ "index out of bounds:" ++ show i
   | otherwise = ccs !! i

getBigCoords :: Int -> Int -> Size -> SmallWorld -> ConnectedComponent -> Coords Pos
getBigCoords !bigIndex !blockSize (Size nBigRows nBigCols) (SmallWorld (SmallMatInfo _ smallMat) (SmallWorldTopology _ resolver)) component =
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

data GraphNode = Node {
    _nodeId              :: {-# UNPACK #-} !Word16
  , _packedNeighboursIds :: {-# UNPACK #-} !Word64 -- 4 Word16 where -1 == no neighbour
}
data GraphCreationState = GC {
    _listNodes :: [GraphNode]
  , _nMinComps :: {-# UNPACK #-} !ComponentCount
  , _canContinue :: {-# UNPACK #-} !Int -- 0 : stop, 2: continue 1 : continue , one multi
}
{-# INLINE mkGraphCreationState #-}
mkGraphCreationState :: GraphCreationState
mkGraphCreationState = GC [] 0 2

-- | Creates an undirected graph, and returns a lower bound of the number of components:
-- we count the mono-node components while creating the graph, and add 1 to that number
-- if there is at least one multi-node component.
mkGraphWithStrictlyLess :: ComponentCount
                        -- ^ If during graph creation we detect that the graph has at least
                        -- that number of components, we cancel graph creation and return Nothing,
                        -- along with that number.
                        -> SmallMatInfo
                        -> ByteArray
                        -- ^ Its size is expected to be >= 'matrix area' * 8
                        -- where 8 is the size of Word64 in bytes.
                        -> (Maybe Undirected.Graph, ComponentCount)
                        -- ^ (Undirected graph, lower bound of components count)
mkGraphWithStrictlyLess !tooBigNComps (SmallMatInfo nAirKeys mat) v =
  (mayGraph, nMinCompsFinal)
 where
  !nRows = Cyclic.nrows mat
  !nCols = Cyclic.ncols mat

  mayGraph =
    if canContinue > 0
      then
        Just $ Undirected.Graph (fromIntegral nAirKeys) $ runST $ do
          -- 8 is size of Word64 in bytes
          mv <- unsafeThawByteArray v
          forM_ listNodes (\(Node key neighbours) -> writeByteArray mv (fromIntegral key) neighbours)
          unsafeFreezeByteArray mv
      else
        Nothing

  (GC listNodes nMinCompsFinal canContinue) =
    List.foldl' (\res'@(GC _ _ continue') row ->
      bool res'
        (let !iRow = row * nCols
         in List.foldl' (\res@(GC ln nMinComps continue) col ->
              bool res
                (let !matIdx = iRow + col
                 in isAir
                      res
                      (\k ->
                       let neighbours = neighbourAirKeys matIdx row col
                           isMono = (neighbours == 0xFFFFFFFFFFFFFFFF)
                           newNMinComps
                             | isMono || continue == 2 {-not oneMulti-} = 1+nMinComps
                             | otherwise = nMinComps
                           willContinue
                             | newNMinComps >= tooBigNComps = 0
                             | isMono = continue
                             | otherwise = 1
                           newList
                             | willContinue > 0 = Node k neighbours:ln
                             | otherwise = [] -- drop the list if we don't continue
                       in GC newList newNMinComps willContinue)
                      $ Cyclic.unsafeGetByIndex matIdx mat)
                $ continue /= 0)
            res'
            [0..nCols-1])
        $ continue' /= 0)
      mkGraphCreationState
      [0..nRows-1]

  neighbourAirKeys :: Int -> Int -> Int -> Word64
  neighbourAirKeys matIdx row col = toWord64 four
   where
    --f 0 = 0xFFFF -- in Graph.Undirected, 0xFFFF means no neighbour ...
    f x = case Cyclic.unsafeGetByIndex x mat of
      MaterialAndKey k -> k -- ... hence this is valid because wall is 0xFFFF
    toWord64 (Four w1 w2 w3 w4) =
      fromIntegral w1 .|.
      (fromIntegral w2 `shiftL` 16) .|.
      (fromIntegral w3 `shiftL` 32) .|.
      (fromIntegral w4 `shiftL` 48)

    four = Four
      -- Benchmarks showed that it is faster to write all directions at once,
      -- rather than just 2, and wait for other directions to come from nearby nodes.
      (bool (f (matIdx-nCols)) 0xFFFF $ row == 0)       -- Up
      (bool (f (matIdx-1)    ) 0xFFFF $ col == 0)       -- LEFT
      (bool (f (matIdx+1)    ) 0xFFFF $ col == nCols-1) -- RIGHT
      (bool (f (matIdx+nCols)) 0xFFFF $ row == nRows-1) -- Down

data Four = Four {-# UNPACK #-} !Word16 {-# UNPACK #-} !Word16 {-# UNPACK #-} !Word16 {-# UNPACK #-} !Word16

mkSpaceFromMat :: Size -> [[Material]] -> Space
mkSpaceFromMat s matMaybeSmaller =
  Space $ Unboxed.fromLists $ extend s matMaybeSmaller

extend :: Size -> [[a]] -> [[a]]
extend (Size rs cs) mat =
  extend' (fromIntegral rs) $ map (extend' $ fromIntegral cs) mat

extend' :: Int -> [a] -> [a]
extend' _ [] = error "extend empty list not supported"
extend' sz l@(_:_) =
  replicate addsLeft (Unsafe.head l) ++
  l ++
  replicate addsRight (Unsafe.last l)
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
