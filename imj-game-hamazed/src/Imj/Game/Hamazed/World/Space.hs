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
    , minCountAirBlocks
    , minCountWallBlocks
    , mkSmallWorld
    , mkSmallMat
    , tryRotationsIfAlmostMatches
    , matchTopology
    , getComponentCount
    , TopoMatch
    ) where

import           Imj.Prelude

import           Data.Either(partitionEithers, isLeft)
import           Data.Graph(Graph, Vertex, graphFromEdges, components)
import           Data.List(length, sortOn)
import qualified Data.List as List (foldl')
import           Data.Maybe(mapMaybe)
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map(alter, insert, insertWith, empty, lookup, toAscList, fromDistinctAscList)
import           Data.Set(Set)
import qualified Data.Set as Set(size, empty, fromList, toList, union)
import           Data.Text(pack)
import           Data.Tree(flatten)
import qualified Data.Vector.Unboxed as V (fromList, length, (!), foldl')
import           System.Random.MWC(GenIO, uniform, uniformR)

import           Imj.Data.Matrix.Unboxed(fromLists)
import           Imj.Data.Matrix.Cyclic(produceRotations, produceUsefullInterleavedVariations)
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
                      -> IO (MkSpaceResult BigWorld, Maybe Statistics)
mkRandomlyFilledSpace _ s 0 _ _ = return (Success $ mkFilledSpace s,Nothing)
mkRandomlyFilledSpace (WallDistribution blockSize wallAirRatio) s nComponents continue gen
  | blockSize <= 0 = fail $ "block size should be strictly positive : " ++ show blockSize
  | otherwise = mkSmallWorld gen (bigToSmall s blockSize) nComponents wallAirRatio continue >>= \(res, stats) ->
  return
    (case res of
      NeedMoreTime -> NeedMoreTime
      Impossible bounds -> Impossible bounds
      Success small -> Success $ smallWorldToBigWorld s blockSize small
    , Just stats)

smallWorldToBigWorld :: Size
                     -> Int
                     -> SmallWorld
                     -> BigWorld
smallWorldToBigWorld s blockSize (SmallWorld smallWorldMat smallTopo) =
  BigWorld (mkSpaceFromMat s innerMat) (smallToBig blockSize s smallTopo)
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
             -> Size
             -- ^ Size of the small world
             -> ComponentCount
             -- ^ Count connex components
             -> Float
             -- ^ Wall / Air ratio
             -> IO Bool
             -- ^ Can continue?
             -> IO (MkSpaceResult SmallWorld, Statistics)
             -- ^ the "small world"
mkSmallWorld gen s nComponents' wallAirRatio continue
  | nComponents' == 0 = error "should be handled by caller"
  | otherwise = either
      (\_ ->
        return (Impossible [pack $ show lowerBounds], zeroStats))
      (\_ -> withDuration (go zeroStats) >>= \((r,stats),dt) ->
        return (case r of
                [] -> NeedMoreTime
                small:_ -> Success small
              , stats { totalTime = dt }))
      $ diagnostic lowerBounds
 where
  lowerBounds = mkLowerBounds s nComponents'
  go stats = continue >>= bool
    (return ([], stats))
      -- We use variations of the matrix to recycle random numbers, as random number generation is expensive.
    (takeWhilePlus isLeft . concatMap -- stop at the first success.
      (tryRotationsIfAlmostMatches matchTopology nComponents) . produceUsefullInterleavedVariations
          <$> mkSmallMat gen wallAirRatio s >>= \l -> do
      let !newStats = updateStats l stats
      case partitionEithers l of
        (_,[]) -> go newStats
        (_,successes) -> return (successes, newStats))
  nComponents = min nComponents' maxNComp -- relax the constraint on number of components if the size is too small
  maxNComp = ComponentCount $ succ $ quot (pred $ area s) 2 -- for checkerboard-like layout
  updateStats l (Statistics i j dt) =
    let i' = i + length l
        j' = List.foldl'
          (flip $ Map.alter (Just . succ . fromMaybe 0))
          j
          $ mapMaybe getComponentCount l
    in Statistics i' j' dt

type TopoMatch = Either (Maybe ComponentCount) SmallWorld

getComponentCount :: TopoMatch -> Maybe ComponentCount
getComponentCount (Left c) = c
getComponentCount (Right (SmallWorld _ topo)) = Just $ ComponentCount $ length $ getConnectedComponents topo

-- Indicates if there is a /real/ wall (i.e not an out of bounds position) in a given direction
data OrthoWall = OrthoWall {
    _dir :: !Direction
    -- ^ The direction in which to search
  , _p1 :: !(Maybe (Coords Pos))
    -- ^ 'Just' if a real wall is at distance 1 in the given 'Direction'.
}

matchTopology :: ComponentCount
              -> Cyclic.Matrix Material
              -> TopoMatch
matchTopology nComponents r
  | not $ smallMatHasAirOnEveryFronteer r = Left Nothing
  | nComponents /= nComps = Left $ Just nComps
    -- from here on, comps is evaluated.
  | not wellDistributed   = Left $ Just nComps
    -- from here on, if the number of components is > 1, we compute the distances between components
  | not spaceIsWellUsed   = Left $ Just nComps
  | otherwise = Right $ SmallWorld r $ SmallWorldTopology comps vtxToCoords
 where
  nComps = ComponentCount $ length gcomps
  gcomps = components graph

  -- complexity of vtxToCoords' is O(1)
  -- complexity of coordsToMaybeVertex is O(log N)
  (graph, vtxToCoords', coordsToMaybeVertex) = graphOfIndex Air r
  vtxToCoords vtx = a where (a,_,_) = vtxToCoords' vtx
  comps = map (ConnectedComponent . V.fromList . flatten) gcomps
  lengths = map countSmallCCElts comps
  wellDistributed = maximum lengths < 2 * minimum lengths

  smallMatHasAirOnEveryFronteer :: Cyclic.Matrix Material -> Bool
  smallMatHasAirOnEveryFronteer mat =
    all (V.foldl' (\res x -> res || (x == Air)) False) fronteers
   where
    fronteers =
      [ Cyclic.getRow 0 mat
      , Cyclic.getRow (pred $ Cyclic.nrows mat) mat
      , Cyclic.getCol 0 mat
      , Cyclic.getCol (pred $ Cyclic.ncols mat) mat
      ]

  {- Returns True if the nearby graph (where an edge means 2 components are nearby)
  has only one connected component.

  Two components are nearby when they are either:

    * vertically separated by a single wall
    * horizontally separated by a single wall
    * diagonally adjacent

  Complexity : nVertex * lg nVertex.

  We traverse the space row by row to optimize memory access locality.

  TODO optimize complexity constant, by:

  * TODO Prune redundant lookups :
    * orthogonal lookups can be done in 2 directions (LEFT, Down) instead of 4
    * diagonal lookups can be done in 2 directions (Down LEFT, Down RIGHT) instead of 4
  * TODO Change Material to RawMaterial (or Int where 0 is a wall, 1 is empty space), and using
    Material = Wall | Air (Maybe !ComponentIdx)
     with unpack instance:
      Wall        -> -2
      Air Nothing -> -1
      Air i       -> i
    Create the Material matrix from the RawMaterial matrix + components lookups,
    only once we know the number of components is right and the components are well distributed.
  -}
  spaceIsWellUsed
    | nComps <= 1 = True
    | otherwise = length compsOfCloseComps == 1
    where
      compsOfCloseComps =
        let (g,_,_) = graphFromEdges $
              map
                (\(i,js) -> (i
                           , i
                           , Set.toList js))
                $ Map.toAscList closeComponentIndices
        in components g

      closeComponentIndices = List.foldl'
        (\edges' rowIdx ->
          -- walls must be detected by /matrix/ lookup in constant time rather than by
          -- 'lookupComponent' in logarithmic time.
          List.foldl' (\edges colIdx -> case Cyclic.unsafeGet rowIdx colIdx r of
            Wall -> edges
            Air -> case neighbourComponents of
              [] -> edges
              l -> Map.insertWith Set.union component (Set.fromList l) edges
             where
              neighbourComponents =
                filter
                  (/= component)
                  $ catMaybes
                  $ lookupNearbyComponentsDiagonally ++ lookupNearbyComponentsOrthogonally

              component = fromMaybe (error "logic") $ lookupComponent pos
              pos = Coords (fromIntegral rowIdx) (fromIntegral colIdx)

              lookupNearbyComponentsDiagonally =
                go $ take 5 $ cycle cyclicOrthoWalls
               where
                go []  = []
                go [_] = []
                go (OrthoWall d1 (Just _) : rest@(OrthoWall _ (Just w2) : _)) =
                  let diag@(Coords (Coord diagRow) (Coord diagCol)) = translateInDir d1 w2 -- == translateInDir d2 w1
                      -- By construction, diag is withinBounds because both OrthoWalls are.
                      -- Hence we skip the 'withinBounds' test.
                  in bool (lookupComponent diag) Nothing (isWall diagRow diagCol): go rest
                go (_:rest@(_:_)) = go rest

              lookupNearbyComponentsOrthogonally = map lookupOrthogonally cyclicOrthoWalls

              lookupOrthogonally (OrthoWall _ Nothing) = Nothing
              lookupOrthogonally (OrthoWall dir (Just wall1Pos))
               | withinBounds2 && not wall2 = lookupComponent p2
               | otherwise = Nothing
               where
                p2@(Coords (Coord r2) (Coord c2)) = translateInDir dir wall1Pos
                withinBounds2 = withinBounds r2 c2
                wall2 = isWall r2 c2

              cyclicDirs = [LEFT,Up,RIGHT,Down]
              cyclicOrthoWalls = map orthoWallInDir cyclicDirs

              orthoWallInDir :: Direction -> OrthoWall
              orthoWallInDir d =
                let p@(Coords (Coord row) (Coord col)) = translateInDir d pos
                in OrthoWall d $ bool
                  Nothing
                  (Just p)
                  $ withinBounds row col && isWall row col

              withinBounds row col = row >= 0 && col >= 0 && row < nRows && col < nCols

              isWall row col = Cyclic.unsafeGet row col r == Wall)
            edges'
            [0..nCols-1])
        -- initialize with every component index
        (Map.fromDistinctAscList $
          zip
            [0..fromIntegral $ nComps-1]
            $ repeat Set.empty :: Map ComponentIdx (Set ComponentIdx))
        [0..nRows-1]
      lookupComponent :: Coords Pos -> Maybe ComponentIdx
      lookupComponent pos = maybe
        -- At pos we have a Wall, or pos is outside world bounds.
        -- For performance reasons, this should be checked ,
        -- by the caller who has the matrix, in O(1), to avoid this O(log nVertex) binary search.
        (error "performance : please do the wall filtering yourself")
        (\vtx -> Just $ fromMaybe (error "logic") $ Map.lookup vtx vertexMap)
        $ coordsToMaybeVertex pos

  -- O(nVertex * log nVertex)
  vertexMap :: Map Vertex ComponentIdx
  vertexMap =
    List.foldl'
      (\m (i, ConnectedComponent v) ->
        V.foldl'
          (\m' value -> Map.insert value i m')
          m
          v)
      Map.empty
      $ zip [0 :: ComponentIdx ..] comps

  nRows = Cyclic.nrows r
  nCols = Cyclic.ncols r

tryRotationsIfAlmostMatches :: (ComponentCount -> Cyclic.Matrix Material -> TopoMatch)
                            -> ComponentCount
                            -> Cyclic.Matrix Material
                            -> [TopoMatch]
tryRotationsIfAlmostMatches matchTopo n m =
  go $ matchTopo n m
 where
  go r@(Right _) = [r]
  go r@(Left Nothing) = [r]
  go r@(Left (Just nComps))
   | abs (n - nComps) <= 5 = -- TODO Maybe the bound should be randomized? At least, fine-tune 5, by finding the sweet spot that gives the more valid worlds per seconds.
     -- we are already close to the target number, so
     -- there is a good probability that rotating will trigger the component match.
     map (matchTopo n) $ drop 1 $ produceRotations m -- skip zero rotation, which has already been tested
   | otherwise = [r]

mkSmallMat :: GenIO
           -> Float
           -- ^ Probability to generate a wall
           -> Size
           -- ^ Size of the matrix
           -> IO (Cyclic.Matrix Material)
mkSmallMat gen wallAirRatio (Size nRows nCols) =
  Cyclic.fromList (fromIntegral nRows) (fromIntegral nCols) <$>
    replicateM (fromIntegral nRows * fromIntegral nCols) (randBiasedMaterial gen wallAirRatio)


{-# INLINE countSmallCCElts #-}
countSmallCCElts :: ConnectedComponent -> Int
countSmallCCElts (ConnectedComponent v) = V.length v

{-# INLINE countBigCCElts #-}
countBigCCElts :: Int -> ConnectedComponent -> Int
countBigCCElts blockSize c =
  blockSize * blockSize * countSmallCCElts c -- doesn't include extensions.

getSmallCoords :: Int -> (Vertex -> Coords Pos) -> ConnectedComponent -> Coords Pos
getSmallCoords smallIndex resolver c@(ConnectedComponent v)
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

smallToBig :: Int -> Size -> SmallWorldTopology -> BigWorldTopology
smallToBig blockSize bigSize (SmallWorldTopology ccs resolve) =
  BigWorldTopology l (countBigCCElts blockSize . safeGetCC) coords
 where
  !l = length ccs
  coords i eltIdx =
    getBigCoords eltIdx blockSize bigSize resolve $ safeGetCC i
  safeGetCC (ComponentIdx i)
   | i < 0 || i >= l = error $ "index out of bounds:" ++ show i
   | otherwise = ccs !! i

getBigCoords :: Int -> Int -> Size -> (Vertex -> Coords Pos) -> ConnectedComponent -> Coords Pos
getBigCoords bigIndex blockSize bigSize@(Size nBigRows nBigCols) resolver component =
  let modulo = blockSize * blockSize
      (smallIndex, remain) = bigIndex `quotRem` modulo
      (remainRow, remainCol) = remain `quotRem` blockSize
      smallCoords = getSmallCoords smallIndex resolver component
      (Size nSmallRows nSmallCols) = bigToSmall bigSize blockSize
      (rowStart, _) = extend'' (fromIntegral nBigRows) $ fromIntegral nSmallRows * blockSize
      (colStart, _) = extend'' (fromIntegral nBigCols) $ fromIntegral nSmallCols * blockSize
      bigUpperLeft = Coords (fromIntegral rowStart) (fromIntegral colStart)
  in translate bigUpperLeft $
       translate (Coords (fromIntegral remainRow) (fromIntegral remainCol)) $
       multiply blockSize smallCoords

graphOfIndex :: Material
             -> Cyclic.Matrix Material
             -> (Graph,
                 Vertex -> (Coords Pos,
                            Coords Pos,
                            [Coords Pos]),
                 Coords Pos -> Maybe Vertex)
graphOfIndex material mat =
  graphFromEdges $ map (\c -> (c, c, connectedNeighbours c)) coords
 where
  -- graphFromEdges sorts edges by keys (i.e Coords Pos here). To optimize this sort,
  -- we make coords ascending by iterating on columns in the /inner/ loop:
  coords = [Coords (Coord r) (Coord c) | r <- [0..pred nRows],
                                         c <- [0..pred nCols],
                                         Cyclic.unsafeGet r c mat == material]

  nRows = Cyclic.nrows mat
  nCols = Cyclic.ncols mat

  connectedNeighbours :: Coords Pos -> [Coords Pos]
  connectedNeighbours pos =
    mapMaybe
      (\other@(Coords (Coord r) (Coord c)) -> bool
        (Just other)
        Nothing $
        r < 0 || c < 0 || r >= nRows || c >= nCols || Cyclic.unsafeGet r c mat /= material)
      [translateInDir LEFT pos, translateInDir Down pos]

mkSpaceFromMat :: Size -> [[Material]] -> Space
mkSpaceFromMat s matMaybeSmaller =
  Space $ fromLists $ extend s matMaybeSmaller

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

randBiasedMaterial :: GenIO -> Float -> IO Material
randBiasedMaterial gen r =
  (\v -> intToMat $ bool 1 0 $ v < r) <$> uniform gen

mkLowerBounds :: Size
              -> ComponentCount
              -> LowerBounds
              -- ^ Nothing is returned when no such world can be generated.
mkLowerBounds sz n =
  LowerBounds diagnose mayMinAir mayMinWall total
 where
  mayMinAir = minCountAirBlocks n sz
  mayMinWall = minCountWallBlocks n sz
  total = area sz
  sizeMsg = pack (show sz) <> " is too small to contain " <> pack (show n)
  diagnose =
    maybe
      (Left sizeMsg)
      (\minAir ->
        maybe
          (Left sizeMsg)
          (\minWall -> bool (Left sizeMsg) (Right ()) $ minAir + minWall <= total)
          mayMinWall)
      mayMinAir

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


intToMat :: Int -> Material
intToMat 0 = Wall
intToMat 1 = Air
intToMat _ = error "unexpected"

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
