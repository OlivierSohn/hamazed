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
    -- for tests
    , mkSmallWorld
    -- * Reexports
    , module Imj.Graphics.Render
    ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)
import qualified Data.Vector.Unboxed as UnboxV (Vector, fromList, length, (!))
import           Data.Graph(Graph, Vertex, graphFromEdges, components)
import           Data.List(length, group, concat, mapAccumL, sortOn)
import           Data.Maybe(mapMaybe)
import qualified Data.Map as Map(empty, alter)
import qualified Data.Set as Set(size, fromList, toList, union)
import           Data.Tree(Tree, flatten)
import           Data.Vector.Unboxed((!), foldl')
import           System.Random.MWC(GenIO, uniform, uniformR)

import           Imj.Data.Matrix.Unboxed(Matrix, getRow, getCol, nrows, ncols, fromLists, nrows, ncols, toLists)
import qualified Imj.Data.Matrix.Unboxed as Mat(fromList)

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
  let rowIndexes = [1..fromIntegral nRows] -- indexes start at 1 in Data.Matrix
  in map (\rowIdx -> do
    let row = getRow (fromIntegral rowIdx) mat -- this is O(1)
    f (pred rowIdx) (\c -> row ! fromIntegral c)) rowIndexes

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
                      -- ^ Returns false to stop
                      -> GenIO
                      -> IO (Maybe (Space, BigWorldTopology), Maybe Statistics)
mkRandomlyFilledSpace _ s 0 _ _ =
  let (a,b) = mkFilledSpace s
  in return (Just (a,b),Nothing)
mkRandomlyFilledSpace (RandomParameters blockSize wallAirRatio strategy) s nComponents continue gen = do
  (mayWT, stats) <-
    mkSmallWorld gen (bigToSmall s blockSize) strategy nComponents wallAirRatio continue
  return (maybe
    Nothing
    (\(MaterialMatrix smallWorldMat, smallTopo) ->
      let replicateElems = replicateElements blockSize
          innerMat = replicateElems $ map replicateElems smallWorldMat
      in Just (mkSpaceFromMat s $ MaterialMatrix innerMat, smallToBig blockSize s smallTopo))
    mayWT, Just stats)

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
             -> Strategy
             -> ComponentCount
             -- ^ Count connex components
             -> Double
             -- ^ Wall / Air ratio
             -> IO Bool
             -- ^ Can continue?
             -> IO (Maybe (MaterialMatrix, SmallWorldTopology), Statistics)
             -- ^ the "small world"
mkSmallWorld gen s strategy nComponents' wallAirRatio' continue = do
  when (nComponents' == 0) $ error "should be handled by caller"
  t <- getSystemTime
  go 0 Map.empty t
 where
  !wallAirRatio = realToFrac wallAirRatio'
  nComponents = min nComponents' maxNComp -- relax the constraint on number of components if the size is too small
  maxNComp = ComponentCount $ succ $ quot (pred $ area s) 2 -- for checkerboard-like layout
  go i j t = do
    (maySmallMat, nMatGenerated) <- mkSmallMatWithAirOnEveryFronteer gen wallAirRatio s continue
    let i' = i + nMatGenerated
    maybe
      (do
        t' <- getSystemTime
        return (Nothing, Statistics i' j $ t...t'))
      (\smallMat -> do
        let (graph, vtxToCoords', _) = graphOfIndex Air smallMat
            vtxToCoords vtx = a where (a,_,_) = vtxToCoords' vtx
        case strategy of
          OneComponentPerShip -> do
            let gcomps = components graph
                nComps = ComponentCount $ length gcomps
                comps = map mkConnectedComponent $ components graph
                j' = Map.alter (Just . succ . fromMaybe 0) nComps j
            if nComponents == nComps && wellDistributed comps
              then do
                t' <- getSystemTime
                return (Just (MaterialMatrix $ toLists smallMat -- TODO use Matrix in MaterialMatrix
                      , SmallWorldTopology comps vtxToCoords), Statistics i' j' $ t...t')
              else
                go i' j' t)
      maySmallMat
  wellDistributed comps =
    maximum lengths < 2 * minimum lengths
   where lengths = map countSmallCCElts comps

mkSmallMatWithAirOnEveryFronteer :: GenIO -> Float -> Size -> IO Bool -> IO (Maybe (Matrix Material), Int)
mkSmallMatWithAirOnEveryFronteer gen wallAirRatio s continue = go 0
 where
  go i = continue >>= \goOn -> if goOn
    then
      mkSmallMat gen wallAirRatio s >>= \candidate ->
        if smallMatHasAirOnEveryFronteer candidate
          then
            return (Just candidate, succ i)
          else
            go $ succ i
    else
      return (Nothing, i)

smallMatHasAirOnEveryFronteer :: Matrix Material -> Bool
smallMatHasAirOnEveryFronteer smallMat =
  all (foldl' (\res x -> res || (x == Air)) False) $ fronteers smallMat
 where
  fronteers mat = [getRow 1 mat, getCol 1 mat, getCol (ncols mat) mat, getRow (nrows mat) mat]

mkSmallMat :: GenIO
           -> Float
           -- ^ Probability to generate a wall
           -> Size
           -- ^ Size of the matrix
           -> IO (Matrix Material)
mkSmallMat gen wallAirRatio (Size nRows nCols) =
  Mat.fromList (fromIntegral nRows) (fromIntegral nCols) <$>
    replicateM (fromIntegral nRows * fromIntegral nCols) (randBiasedMaterial gen wallAirRatio)

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
  let sz@(nRows,nCols) = getMatSize mat
      coords = [Coords (Coord r) (Coord c) | c <- [0..pred nCols],
                                             r <- [0..pred nRows],
                                             at mat r c == matchIdx]
  in graphFromEdges $ map (\c -> (c, c, connectedNeighbours matchIdx c mat sz)) coords


connectedNeighbours :: Material -> Coords Pos -> Matrix Material -> (Int, Int) -> [Coords Pos]
connectedNeighbours match coords mat (nRows,nCols) =
  let neighbours = [translateInDir LEFT coords, translateInDir Down coords]
  in mapMaybe
      (\other@(Coords (Coord r) (Coord c)) ->
        if r < 0 || c < 0 || r >= nRows || c >= nCols || at mat r c /= match
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
