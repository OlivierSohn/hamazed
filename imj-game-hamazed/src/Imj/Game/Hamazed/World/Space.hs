{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Imj.Game.Hamazed.World.Space
    ( Space
    , toListOfLists
    , fromListOfLists
    , BigWorldTopology(..)
    , ComponentIdx
    , getComponentIndices
    , Material(..)
    , materialColor
    , materialGlyph
    , mkEmptySpace
    , mkFilledSpace
    , mkRandomlyFilledSpace
    , mkRenderedSpace
    , RandomParameters(..)
    , Strategy(..)
    , location
    , distanceToSpace
    , Scope(..)
    , drawSpace
    , mkRandomPosSpeed
    , unsafeGetMaterial
    , getBigCoords
    , countBigCCElts
    , OverlapKind(..)
    , randomCCCoords
    -- * Reexports
    , module Imj.Graphics.Render
    ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)
import qualified Data.Vector.Unboxed as UnboxV (Vector, fromList, length, (!))
import           Data.Graph(Graph, Vertex, graphFromEdges, components)
import           Data.List(elem, length, group, concat, mapAccumL, sortOn)
import           Data.Maybe(mapMaybe)
import           Data.Matrix(Matrix, getElem, fromLists, getMatrixAsVector, nrows, ncols, toLists)
import qualified Data.Set as Set(size, fromList, toList, union)
import           Data.Tree(Tree, flatten)
import           Data.Vector(slice, (!))

import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.Font
import           Imj.Graphics.Render
import           Imj.Physics.Discrete
import           Imj.Util

toListOfLists :: Space -> MaterialMatrix
toListOfLists (Space mat) = MaterialMatrix $ toLists mat

fromListOfLists :: MaterialMatrix -> Space
fromListOfLists (MaterialMatrix m) = Space $ fromLists m

-- | Creates a 'PosSpeed' from a position,
-- moves to precollision and mirrors speed if a collision is detected for
-- the next step (see 'mirrorSpeedAndMoveToPrecollisionIfNeeded').
mkRandomPosSpeed :: Space
                 -> Coords Pos
                 -- ^ Precondition : is not colliding
                 -> IO PosSpeed
mkRandomPosSpeed space pos =
  fst
    . mirrorSpeedAndMoveToPrecollisionIfNeeded (`location` space)
    . PosSpeed pos <$> randomSpeed

oneRandom :: Int -> Int -> IO Int
oneRandom a b =
  head <$> randomRsIO a b

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

randomCCCoords :: Int -> ComponentIdx -> BigWorldTopology -> OverlapKind -> IO [Coords Pos]
randomCCCoords nPositions idxCC topo = \case
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
          . map (getEltCoords topo idxCC) . take n <$> genRandomEltIndex
    return (startIndex + n,l)

  genRandomEltIndex = randomInts $ getComponentSize topo idxCC

randomSpeed :: IO (Coords Vel)
randomSpeed = do
    x <- rnd
    y <- rnd
    return $ Coords (Coord x) (Coord y)
  where
    rnd = oneRandom (-1) 1

randomInts :: Int -> IO [Int]
randomInts =
  randomRsIO 0 . pred

forEachRowPure :: Matrix Material
               -> Size
               -> (Coord Row -> (Coord Col -> Material) -> b)
               -> [b]
forEachRowPure mat (Size nRows nColumns) f =
  let rowIndexes = [0..fromIntegral $ nRows-1] -- index of inner row
      matAsOneVector = getMatrixAsVector mat -- this is O(1)
  in map (\rowIdx -> do
    let startIdx = fromIntegral rowIdx * fromIntegral nColumns :: Int
        row = slice startIdx (fromIntegral nColumns) matAsOneVector
    f rowIdx (\c -> row ! fromIntegral c)) rowIndexes

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
mkRandomlyFilledSpace :: RandomParameters -> Size -> Int -> IO (Space, BigWorldTopology)
mkRandomlyFilledSpace _ s 0 = return $ mkFilledSpace s
mkRandomlyFilledSpace (RandomParameters blockSize strategy) s nComponents = do
  (MaterialMatrix smallWorldMat, smallTopo) <- mkSmallWorld (bigToSmall s blockSize) strategy nComponents
  let replicateElems = replicateElements blockSize
      innerMat = replicateElems $ map replicateElems smallWorldMat
  return (mkSpaceFromMat s $ MaterialMatrix innerMat, smallToBig blockSize s smallTopo)

bigToSmall :: Size -> Int -> Size
bigToSmall (Size heightEmptySpace widthEmptySpace) blockSize =
  let nCols = quot widthEmptySpace $ fromIntegral blockSize
      nRows = quot heightEmptySpace $ fromIntegral blockSize
  in Size nRows nCols

--  TODO We could measure, on average, how many tries it takes to generate a graph
--  that meets the requirement for usual values of:
--  - probability of having air vs. a wall at any cell
--  - size of the small world
{- | Generates a random world with the constraint that it should have
a single "Air" connected component. The function recurses and builds a new random world
until the constraint is met.
It might take "a long time" especially if worldsize is big and multFactor is small.
An interesting problem would be to compute the complexity of this function.
To do so we need to know the probability to have a unique connected component in
the random graph defined in the function.
-}
mkSmallWorld :: Size
             -- ^ Size of the small world
             -> Strategy
             -> Int
             -> IO (MaterialMatrix, SmallWorldTopology)
             -- ^ the "small world"
mkSmallWorld s@(Size nRows nCols) strategy nComponents' = do
  when (nComponents' == 0) $ error "should be handled by caller"
  go
 where
  nComponents = min nComponents' maxNComp -- relax the constraint on number of components if the size is too small
  maxNComp = succ $ quot (pred $ area s) 2 -- for checkerboard-like layout
  go = do
    smallMat <- mkSmallMat
    let (graph, vtxToCoords', _) = graphOfIndex Air $ fromLists smallMat
        vtxToCoords vtx = a where (a,_,_) = vtxToCoords' vtx
    case strategy of
      OneComponentPerShip -> do
        let comps = map mkConnectedComponent $ components graph
        if nComponents == length comps && wellDistributed comps
          then
            return (MaterialMatrix smallMat, SmallWorldTopology comps vtxToCoords)
          else
            go
  mkSmallMat = mapM mkRandomRow [0..nRows-1] >>= \smallMat ->
    if all (elem Air) $ fronteers smallMat
      then
        return smallMat
      else
        mkSmallMat
  mkRandomRow _ = take (fromIntegral nCols) <$> randMaterial -- TODO use a Matrix directly
  fronteers mat = [head mat, last mat, map head mat, map last mat]
  wellDistributed comps =
    maximum lengths < 2 * minimum lengths
   where lengths = map countSmallCCElts comps

data BigWorldTopology = BigWorldTopology {
    countComponents :: !Int
  , getComponentSize :: ComponentIdx -> Int
  , getEltCoords :: ComponentIdx -> Int -> Coords Pos
}

getComponentIndices :: BigWorldTopology -> [ComponentIdx]
getComponentIndices t = map ComponentIdx [0 .. pred $ countComponents t]

newtype ComponentIdx = ComponentIdx Int
  deriving(Generic, Eq, Ord, Show, Binary)

data SmallWorldTopology = SmallWorldTopology {
    _connectedComponents :: [ConnectedComponent]
  , _resolver :: Vertex -> Coords Pos
  -- ^ Used to get 'ConnectedComponent''s coordinates w.r.t small world
}

newtype ConnectedComponent = ConnectedComponent (UnboxV.Vector Vertex)
  deriving(Generic, Show)


{-# INLINE countSmallCCElts #-}
countSmallCCElts :: ConnectedComponent -> Int
countSmallCCElts (ConnectedComponent v) = UnboxV.length v

{-# INLINE mkConnectedComponent #-}
mkConnectedComponent :: Tree Vertex -> ConnectedComponent
mkConnectedComponent =
  ConnectedComponent . UnboxV.fromList . flatten

{-# INLINE countBigCCElts #-}
countBigCCElts :: Int -> ConnectedComponent -> Int
countBigCCElts blockSize c =
  blockSize * blockSize * countSmallCCElts c -- doesn't include extensions.

getSmallCoords :: Int -> (Vertex -> Coords Pos) -> ConnectedComponent -> Coords Pos
getSmallCoords smallIndex resolver c@(ConnectedComponent v)
  | smallIndex < 0 || smallIndex >= countSmallCCElts c = error $ "index out of bounds:" ++ show smallIndex
  | otherwise = resolver $ v UnboxV.! smallIndex

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
             -> Matrix Material
             -> (Graph,
                 Vertex -> (Coords Pos,
                            Coords Pos,
                            [Coords Pos]),
                 Coords Pos -> Maybe Vertex)
graphOfIndex matchIdx mat =
  let sz@(nRows,nCols) = size mat
      coords = [Coords (Coord r) (Coord c) | c <- [0..nCols-1],
                                             r <- [0..nRows-1],
                                             mat `at` (r, c) == matchIdx]
  in graphFromEdges $ map (\c -> (c, c, connectedNeighbours matchIdx c mat sz)) coords

-- these functions adapt the API of matrix to the API of hmatrix
{-# INLINE size #-}
size :: Matrix a -> (Int, Int)
size mat = (nrows mat, ncols mat)

{-# INLINE at #-}
at :: Matrix a -> (Int, Int) -> a
at mat (i, j) =
  getElem (succ i) (succ j) mat -- indexes start at 1 in Data.Matrix

connectedNeighbours :: Material -> Coords Pos -> Matrix Material -> (Int, Int) -> [Coords Pos]
connectedNeighbours match coords mat (nRows,nCols) =
  let neighbours = [translateInDir LEFT coords, translateInDir Down coords]
  in mapMaybe
      (\other@(Coords (Coord r) (Coord c)) ->
        if r < 0 || c < 0 || r >= nRows || c >= nCols || mat `at` (r, c) /= match
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

randMaterial :: IO [Material]
randMaterial = map intToMat <$> randomRsIO 0 1

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
  concat $
    forEachRowPure mat s $
      \row accessMaterial ->
          snd $ mapAccumL
                  (\col listMaterials@(material:_) ->
                     let count = length listMaterials
                     in (col + fromIntegral count,
                         DrawGroup (Coords row col) (materialColor material) (materialGlyph material) count))
                  (Coord 0) $ group $ map accessMaterial [0..fromIntegral $ pred cs]

unsafeGetMaterial :: Coords Pos -> Space -> Material
unsafeGetMaterial (Coords (Coord r) (Coord c)) (Space mat) =
  mat `at` (r, c)

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
