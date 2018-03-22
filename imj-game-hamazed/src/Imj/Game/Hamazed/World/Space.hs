{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}

module Imj.Game.Hamazed.World.Space
    ( Space
    , toListOfLists
    , fromListOfLists
    , Material(..)
    , materialColor
    , materialGlyph
    , mkEmptySpace
    , mkFilledSpace
    , mkRandomlyFilledSpace
    , mkRenderedSpace
    , RandomParameters(..)
    , location
    , distanceToSpace
    , Scope(..)
    , drawSpace
    , mkRandomPosSpeed
    , getBigCoords
    , countBigCCElts
    , OverlapKind(..)
    , randomCCCoords
    -- for tests
    , mkSmallWorld
    , mkSmallMat
    , matchTopology
    , getComponentCount
    , TopoMatch
    -- * Reexports
    , module Imj.Graphics.Render
    ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)
import           Data.Either(partitionEithers, isLeft)
import           Data.Graph(Graph, Vertex, graphFromEdges, components)
import           Data.List(length, group, concat, mapAccumL, sortOn)
import qualified Data.List as List (foldl')
import           Data.Maybe(mapMaybe, listToMaybe)
import qualified Data.Map as Map(alter)
import qualified Data.Set as Set(size, fromList, toList, union)
import           Data.Tree(flatten)
import qualified Data.Vector.Unboxed as V (Vector, fromList, length, (!), foldl')
import           System.Random.MWC(GenIO, uniform, uniformR)

import           Imj.Data.Matrix.Unboxed(Matrix, getRow, fromLists, toLists)
import           Imj.Data.Matrix.Cyclic(produceRotations, produceUsefullInterleavedVariations)
import qualified Imj.Data.Matrix.Cyclic as Cyclic(Matrix, fromList, toLists, unsafeGet, getRow, getCol, nrows, ncols)
import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.Font
import           Imj.Graphics.Render
import           Imj.Physics.Discrete
import           Imj.Timing
import           Imj.Util

toListOfLists :: Space -> MaterialMatrix
toListOfLists (Space mat) = MaterialMatrix $ toLists mat

fromListOfLists :: MaterialMatrix -> Space
fromListOfLists (MaterialMatrix m) = Space $ fromLists m

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


forEachRowPure :: Matrix Material
               -> Size
               -> (Coord Row -> (Coord Col -> Material) -> b)
               -> [b]
forEachRowPure mat (Size nRows _) f =
  let rowIndexes = [0..pred $ fromIntegral nRows]
  in map (\rowIdx -> do
    let row = getRow (fromIntegral rowIdx) mat -- this is O(1)
    f rowIdx (\c -> row V.! fromIntegral c)) rowIndexes

-- | Creates a rectangular empty space of size specified in parameters.
mkEmptySpace :: Size -> (Space, BigWorldTopology)
mkEmptySpace s =
  (mkSpaceFromMat s $ MaterialMatrix [[Air]], mkEmptyBigWorldTopology s)

mkFilledSpace :: Size -> (Space, BigWorldTopology)
mkFilledSpace s@(Size heightEmptySpace widthEmptySpace) =
  let w = fromIntegral widthEmptySpace
      h = fromIntegral heightEmptySpace
      l = replicate h $ replicate w Wall
  in (mkSpaceFromMat s $ MaterialMatrix l, mkFilledBigWorldTopology s)

-- | Creates a rectangular random space of size specified in parameters.
-- 'IO' is used for random numbers generation.
mkRandomlyFilledSpace :: RandomParameters
                      -> Size
                      -> ComponentCount
                      -> IO Bool
                      -- ^ Computation stops when it returns False
                      -> GenIO
                      -> IO (Maybe (Space, BigWorldTopology), Maybe Statistics)
mkRandomlyFilledSpace _ s 0 _ _ =
  let (a,b) = mkFilledSpace s
  in return (Just (a,b),Nothing)
mkRandomlyFilledSpace (RandomParameters blockSize wallAirRatio) s nComponents continue gen = do
  (mayWT, stats) <-
    mkSmallWorld gen (bigToSmall s blockSize) nComponents wallAirRatio continue
  return
    (maybe
      Nothing
      (\(MaterialMatrix smallWorldMat, smallTopo) ->
        let replicateElems = replicateElements blockSize
            innerMat = replicateElems $ map replicateElems smallWorldMat
        in Just (mkSpaceFromMat s $ MaterialMatrix innerMat, smallToBig blockSize s smallTopo))
      mayWT
    , Just stats)

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
             -> IO (Maybe (MaterialMatrix, SmallWorldTopology), Statistics)
             -- ^ the "small world"
mkSmallWorld gen s nComponents' wallAirRatio continue
  | nComponents' == 0 = error "should be handled by caller"
  | otherwise = withDuration (go zeroStats) >>= \((r,stats),dt) ->
      return (listToMaybe r, stats { totalTime = dt })
 where
  go stats = continue >>= bool
    (return ([], stats))
      -- We use variations of the matrix to recycle random numbers, as random number generation is expensive.
    (takeWhilePlus isLeft . concatMap -- stop at the first success.
      (\x ->
        let res = matchTopology nComponents x
        in tryRotationsIfAlmostMatches nComponents x res) . produceUsefullInterleavedVariations
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

type TopoMatch = Either (Maybe ComponentCount) (MaterialMatrix, SmallWorldTopology)

getComponentCount :: TopoMatch -> Maybe ComponentCount
getComponentCount (Left c) = c
getComponentCount (Right (_,topo)) = Just $ ComponentCount $ length $ getConnectedComponents topo

matchTopology :: ComponentCount
              -> Cyclic.Matrix Material
              -> TopoMatch
matchTopology nComponents r
  | not $ smallMatHasAirOnEveryFronteer r = Left Nothing
  | nComponents /= nComps = Left $ Just nComps
    -- from here on, comps is evaluated.
  | not wellDistributed   = Left $ Just nComps
  | otherwise = Right (MaterialMatrix $ Cyclic.toLists r, SmallWorldTopology comps vtxToCoords)
 where
  nComps = ComponentCount $ length gcomps
  gcomps = components graph
  (graph, vtxToCoords', _) = graphOfIndex Air r
  vtxToCoords vtx = a where (a,_,_) = vtxToCoords' vtx
  comps = map (ConnectedComponent . V.fromList . flatten) $ components graph
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

tryRotationsIfAlmostMatches :: ComponentCount -> Cyclic.Matrix Material -> TopoMatch -> [TopoMatch]
tryRotationsIfAlmostMatches _ _ r@(Right _) = [r]
tryRotationsIfAlmostMatches _ _ r@(Left Nothing) = [r]
tryRotationsIfAlmostMatches n m r@(Left (Just nComps))
  | abs (n - nComps) <= 5 = -- TODO fine-tune 5, by finding the sweet spot that gives the more valid worlds per seconds.
    -- we are already close to the target number, so
    -- there is a good probability that rotating will trigger the component match.
    map (matchTopology n) $ drop 1 $ produceRotations m -- skip zero rotation, which has already been tested
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

data SmallWorldTopology = SmallWorldTopology {
    getConnectedComponents :: [ConnectedComponent]
  , _resolver :: Vertex -> Coords Pos
  -- ^ Used to get 'ConnectedComponent''s coordinates w.r.t small world
}

newtype ConnectedComponent = ConnectedComponent (V.Vector Vertex)
  deriving(Generic, Show)


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
graphOfIndex matchIdx mat =
  let sz@(nRows,nCols) = (Cyclic.nrows mat, Cyclic.ncols mat)
      coords = [Coords (Coord r) (Coord c) | c <- [0..pred nCols],
                                             r <- [0..pred nRows],
                                             Cyclic.unsafeGet r c mat == matchIdx]
  in graphFromEdges $ map (\c -> (c, c, connectedNeighbours matchIdx c mat sz)) coords


connectedNeighbours :: Material -> Coords Pos -> Cyclic.Matrix Material -> (Int, Int) -> [Coords Pos]
connectedNeighbours material coords mat (nRows,nCols) =
  let neighbours = [translateInDir LEFT coords, translateInDir Down coords]
  in mapMaybe
      (\other@(Coords (Coord r) (Coord c)) ->
        if r < 0 || c < 0 || r >= nRows || c >= nCols || Cyclic.unsafeGet r c mat /= material
          then
            Nothing
          else
            Just other)
      neighbours

mkSpaceFromMat :: Size -> MaterialMatrix -> Space
mkSpaceFromMat s (MaterialMatrix matMaybeSmaller) =
  let ext = extend s matMaybeSmaller
      mat = fromLists ext
  in Space mat

mkRenderedSpace :: Space -> RenderedSpace
mkRenderedSpace s@(Space mat) = RenderedSpace $ matToDrawGroups mat $ getSize s

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
  (\v ->
    if v < r
      then intToMat 0
      else intToMat 1) <$> uniform gen

intToMat :: Int -> Material
intToMat 0 = Wall
intToMat 1 = Air
intToMat _ = error "unexpected"

{-# INLINE materialColor #-}
materialColor :: Material -> LayeredColor
materialColor = \case
  Wall -> wallColors
  Air  -> airColors

{-# INLINE materialGlyph #-}
materialGlyph :: Material -> Glyph
materialGlyph = gameGlyph . (\case
  Wall -> 'Z'
  Air  -> ' ')

matToDrawGroups :: Matrix Material -> Size -> [DrawGroup]
matToDrawGroups mat s@(Size _ cs) =
  concat $ forEachRowPure mat s $ \row accessMaterial ->
    snd $ mapAccumL
      (\col listMaterials@(material:_) ->
         let count = length listMaterials
         in (col + fromIntegral count,
             DrawGroup (Coords row col) (materialColor material) (materialGlyph material) count))
      (Coord 0)
      $ group $ map accessMaterial [0..fromIntegral $ pred cs]

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

{-# INLINABLE drawSpace #-}
drawSpace :: (Draw e, MonadReader e m, MonadIO m)
          => RenderedSpace
          -> Coords Pos
          -- ^ World upper left coordinates w.r.t terminal frame.
          -> m ()
drawSpace (RenderedSpace drawGroups) upperLeft =
  mapM_ (drawGroup upperLeft) drawGroups

{-# INLINABLE drawGroup #-}
drawGroup :: (Draw e, MonadReader e m, MonadIO m)
          => Coords Pos
          -> DrawGroup
          -> m ()
drawGroup worldCoords (DrawGroup pos colors glyph count) =
  drawGlyphs count glyph (sumCoords pos worldCoords) colors
