{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Game.Hamazed.World.Space
    ( Space
    , toListOfLists
    , fromListOfLists
    , Material(..)
    , materialColor
    , materialChar
    , mkEmptySpace
    , mkDeterministicallyFilledSpace
    , mkRandomlyFilledSpace
    , mkRenderedSpace
    , RandomParameters(..)
    , Strategy(..)
    , location
    , distanceToSpace
    , Scope(..)
    , drawSpace
    , createRandomNonCollidingPosSpeed
    , unsafeGetMaterial
    -- * Reexports
    , module Imj.Graphics.Render
    ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)
import           Data.Graph( Graph
                           , graphFromEdges
                           , components )
import           Data.List(length, group, concat, mapAccumL)
import           Data.Maybe(mapMaybe)
import           Data.Matrix( getElem
                            , fromLists
                            , getMatrixAsVector
                            , Matrix
                            , nrows, ncols, toLists )
import           Data.Vector(Vector, slice, (!))

import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Geo.Discrete
import           Imj.Graphics.Render
import           Imj.Physics.Discrete
import           Imj.Util

toListOfLists :: Space -> [[Material]]
toListOfLists (Space mat) = toLists mat

fromListOfLists :: [[Material]] -> Space
fromListOfLists = Space . fromLists

-- | Creates a 'PosSpeed' such that its position is not colliding,
-- and moves to precollision and mirrors speed if a collision is detected for
-- the next step (see 'mirrorSpeedAndMoveToPrecollisionIfNeeded').
createRandomNonCollidingPosSpeed :: Space -> IO PosSpeed
createRandomNonCollidingPosSpeed space = do
  pos <- randomNonCollidingPos space
  dx <- randomSpeed
  dy <- randomSpeed
  return $ fst
    $ mirrorSpeedAndMoveToPrecollisionIfNeeded (`location` space)
    $ PosSpeed pos (Coords (Coord dx) (Coord dy))

oneRandom :: Int -> Int -> IO Int
oneRandom a b =
  head <$> randomRsIO a b

randomSpeed :: IO Int
randomSpeed = oneRandom (-1) 1

randomNonCollidingPos :: Space -> IO (Coords Pos)
randomNonCollidingPos space = do
  coords <- randomCoords $ getSize space
  case getMaterial coords space of
    Wall -> randomNonCollidingPos space
    Air -> return coords

randomInt :: Int -> IO Int
randomInt sz =
  oneRandom 0 (sz-1)

randomCoords :: Size -> IO (Coords Pos)
randomCoords (Size rs cs) =
  Coords <$> (randomCoord $ fromIntegral rs)
         <*> (randomCoord $ fromIntegral cs)

randomCoord :: Coord a -> IO (Coord a)
randomCoord (Coord sz) =
  Coord <$> randomInt sz

forEachRowPure :: Matrix Material -> Size -> (Coord Row -> (Coord Col -> Material) -> b) -> [b]
forEachRowPure mat (Size nRows nColumns) f =
  let rowIndexes = [0..fromIntegral $ nRows-1] -- index of inner row
      matAsOneVector = flatten mat -- this is O(1)
  in map (\rowIdx -> do
    let startIdx = fromIntegral rowIdx * fromIntegral nColumns :: Int
        row = slice startIdx (fromIntegral nColumns) matAsOneVector
    f rowIdx (\c -> row ! fromIntegral c)) rowIndexes

-- | Creates a rectangular empty space of size specified in parameters.
mkEmptySpace :: Size -> Space
mkEmptySpace s =
  mkSpaceFromMat s [[Air]]

-- | Creates a rectangular deterministic space of size specified in parameters.
mkDeterministicallyFilledSpace :: Size -> Space
mkDeterministicallyFilledSpace s@(Size heightEmptySpace widthEmptySpace) =
  let w = fromIntegral widthEmptySpace
      h = fromIntegral heightEmptySpace
      middleRow = replicate w Air
      collisionRow = replicate 2 Air ++ replicate (w-4) Wall ++ replicate 2 Air
      ncolls = 8 :: Int
      nEmpty = h - ncolls
      n1 = quot nEmpty 2
      n2 = nEmpty - n1
      l = replicate n1 middleRow ++ replicate ncolls collisionRow ++ replicate n2 middleRow
  in mkSpaceFromMat s l

-- | Creates a rectangular random space of size specified in parameters, with a
-- one-element border. 'IO' is used for random numbers generation.
mkRandomlyFilledSpace :: RandomParameters -> Size -> IO Space
mkRandomlyFilledSpace (RandomParameters blockSize strategy) s = do
  smallWorldMat <- mkSmallWorld s blockSize strategy

  let replicateElems = replicateElements blockSize
      innerMat = replicateElems $ map replicateElems smallWorldMat
  return $ mkSpaceFromMat s innerMat

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
             -- ^ Size of the big world
             -> Int
             -- ^ Pixel width (if 1, the small world will have the same size as the big one)
             -> Strategy
             -> IO [[Material]]
             -- ^ the "small world"
mkSmallWorld s@(Size heightEmptySpace widthEmptySpace) multFactor strategy = do
  let nCols = quot widthEmptySpace $ fromIntegral multFactor
      nRows = quot heightEmptySpace $ fromIntegral multFactor
      mkRandomRow _ = take (fromIntegral nCols) . map intToMat <$> rands -- TODO use a Matrix directly
  smallMat <- mapM mkRandomRow [0..nRows-1]

  let mat = fromLists smallMat
      graph = graphOfIndex Air mat
  case strategy of
    StrictlyOneComponent -> case components graph of
      [_] -> return smallMat
      _   -> mkSmallWorld s multFactor strategy

graphOfIndex :: Material -> Matrix Material -> Graph
graphOfIndex matchIdx mat =
  let sz@(nRows,nCols) = size mat
      coords = [Coords (Coord r) (Coord c) | c <-[0..nCols-1], r <- [0..nRows-1], mat `at` (r, c) == matchIdx]
      edges = map (\c -> (c, c, connectedNeighbours matchIdx c mat sz)) coords
      (graph, _, _) = graphFromEdges edges
  in graph

-- these functions adapt the API of matrix to the API of hmatrix
size :: Matrix a -> (Int, Int)
size mat = (nrows mat, ncols mat)

flatten :: Matrix a -> Vector a
flatten = getMatrixAsVector

at :: Matrix a -> (Int, Int) -> a
at mat (i, j) =
  getElem (succ i) (succ j) mat -- indexes start at 1 in Data.Matrix

connectedNeighbours :: Material -> Coords Pos -> Matrix Material -> (Int, Int) -> [Coords Pos]
connectedNeighbours match coords mat (nRows,nCols) =
  let neighbours = [translateInDir LEFT coords, translateInDir Down coords]
  in mapMaybe (\other@(Coords (Coord r) (Coord c)) ->
        if r < 0 || c < 0 || r >= nRows || c >= nCols || mat `at` (r, c) /= match
          then
            Nothing
          else
            Just other) neighbours

mkSpaceFromMat :: Size -> [[Material]] -> Space
mkSpaceFromMat s matMaybeSmaller =
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
  let len = length l
      addsTotal = sz - assert (len <= sz) len
      addsLeft = quot addsTotal 2
      addsRight = addsTotal - addsLeft
  in replicate addsLeft (head l) ++ l ++ replicate addsRight (last l)

rands :: IO [Int]
rands = randomRsIO 0 1

intToMat :: Int -> Material
intToMat 0 = Wall
intToMat 1 = Air
intToMat _ = error "unexpected"

{-# INLINE materialColor #-}
materialColor :: Material -> LayeredColor
materialColor = \case
  Wall -> wallColors
  Air  -> airColors

{-# INLINE materialChar #-}
materialChar :: Material -> Char
materialChar = \case
  Wall -> 'Z'
  Air  -> ' '

matToDrawGroups :: Matrix Material -> Size -> [DrawGroup]
matToDrawGroups mat s@(Size _ cs) =
  concat $
    forEachRowPure mat s $
      \row accessMaterial ->
          snd $ mapAccumL
                  (\col listMaterials@(material:_) ->
                     let count = length listMaterials
                     in (col + fromIntegral count,
                         DrawGroup (Coords row col) (materialColor material) (materialChar material) count))
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
drawGroup worldCoords (DrawGroup pos colors char count) =
  drawChars count char (sumCoords pos worldCoords) colors
