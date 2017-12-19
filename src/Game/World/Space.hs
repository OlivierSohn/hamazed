{-# LANGUAGE NoImplicitPrelude #-}

module Game.World.Space
    ( renderSpace
    , getMaterial
    , location
    , strictLocation
    , mkDeterministicallyFilledSpace
    , mkRandomlyFilledSpace
    , mkEmptySpace
    , createRandomPosSpeed
    , locationFunction
    -- * Reexports
    , module Game.World.Types
    , module Draw
    ) where

import           Imajuscule.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)

import           System.Console.Terminal.Size( Window(..) )

import           Data.Graph( Graph
                           , graphFromEdges
                           , components )
import           Data.List(length, group, concat, mapAccumL)
import           Data.Maybe(mapMaybe)
import           Data.Matrix( getElem
                            , fromLists
                            , getMatrixAsVector
                            , Matrix
                            , nrows, ncols )
import           Data.Vector(Vector, slice, (!))

import           Foreign.C.Types( CInt(..) )

import           Draw

import           Game.Color
import           Game.World.Types
import           Game.World.Size

import           Geo.Discrete hiding (extend)

import           Physics.Discrete.Collision

import           Util( replicateElements
                     , randomRsIO )


createRandomPosSpeed :: Space -> IO PosSpeed
createRandomPosSpeed space = do
  pos <- randomNonCollidingPos space
  dx <- randomSpeed
  dy <- randomSpeed
  return $ fst
    $ mirrorIfNeeded (`location` space)
    $ PosSpeed pos (Coords (Coord dx) (Coord dy))

oneRandom :: Int -> Int -> IO Int
oneRandom a b = do
  r <- randomRsIO a b
  return $ head $ take 1 $ r

randomSpeed :: IO Int
randomSpeed = oneRandom (-1) 1

randomNonCollidingPos :: Space -> IO Coords
randomNonCollidingPos space@(Space _ worldSize _) = do
  coords <- randomCoords worldSize
  case getMaterial coords space of
    Wall -> randomNonCollidingPos space
    Air -> return coords

randomInt :: Int -> IO Int
randomInt sz =
  oneRandom 0 (sz-1)

randomCoords :: WorldSize -> IO Coords
randomCoords (WorldSize (Coords rs cs)) = do
  r <- randomCoord rs
  c <- randomCoord cs
  return $ Coords r c

randomCoord :: Coord a -> IO (Coord a)
randomCoord (Coord sz) = Coord <$> randomInt sz

forEachRowPure :: Matrix CInt -> WorldSize -> (Coord Row -> (Coord Col -> Material) -> b) -> [b]
forEachRowPure mat (WorldSize (Coords nRows nColumns)) f =
  let rowIndexes = [0..nRows-1]          -- index of inner row
      internalRowLength = nInternalColumns
      rowLength = internalRowLength - 2
      nInternalColumns = nColumns + 2  -- size of a column in the matrix
      matAsOneVector = flatten mat -- this is O(1)
  in map (\rowIdx -> do
    let internalRowIdx = succ rowIdx
        startInternalIdx = fromIntegral internalRowIdx * fromIntegral nInternalColumns :: Int
        startIdx = succ startInternalIdx
        row = slice startIdx (fromIntegral rowLength) matAsOneVector
    f rowIdx (\c -> mapInt $ row ! fromIntegral c)) rowIndexes

-- unfortunately I didn't find a Matrix implementation that supports arbitrary types
-- so I need to map my type on a CInt
mapMaterial :: Material -> CInt
mapMaterial Air  = 0
mapMaterial Wall = 1

mapInt :: CInt -> Material
mapInt 0 = Air
mapInt 1 = Wall
mapInt _ = error "mapInt arg out of bounds"

mkEmptySpace :: WorldSize -> Space
mkEmptySpace s =
  let air  = mapMaterial Air
  in mkSpaceFromInnerMat s [[air]]

mkDeterministicallyFilledSpace :: WorldSize -> Space
mkDeterministicallyFilledSpace s@(WorldSize (Coords heightEmptySpace widthEmptySpace)) =
  let wall = mapMaterial Wall
      air  = mapMaterial Air

      w = fromIntegral widthEmptySpace
      h = fromIntegral heightEmptySpace
      middleRow = replicate w air
      collisionRow = replicate 2 air ++ replicate (w-4) wall ++ replicate 2 air
      ncolls = 8 :: Int
      nEmpty = h - ncolls
      n1 = quot nEmpty 2
      n2 = nEmpty - n1
      l = replicate n1 middleRow ++ replicate ncolls collisionRow ++ replicate n2 middleRow
  in mkSpaceFromInnerMat s l

-- | creates a rectangle of size specified in parameters, with a one-element border.
--  it uses IO for random numbers
mkRandomlyFilledSpace :: RandomParameters -> WorldSize -> IO Space
mkRandomlyFilledSpace (RandomParameters blockSize strategy) s = do
  smallWorldMat <- mkSmallWorld s blockSize strategy

  let innerMat = replicateElements blockSize $ map (replicateElements blockSize) smallWorldMat
  return $ mkSpaceFromInnerMat s innerMat

-- | This function generates a random world with the constraint that it should have
--   a single "Air" connected component. The function recurses and builds a new random world
--   until the constraint is met.
--  It might take "a long time" especially if worldsize is big and multFactor is small.
--  An interesting problem would be to compute the complexity of this function.
--  To do so we need to know the probability
--  to have a unique connected component in the random graph defined in the function.
--  TODO We could measure, on average, how many tries it takes to generate a graph
--  that meets the requirement for usual values of:
--  - probability of having air vs. a wall at any cell
--  - size of the small world
mkSmallWorld :: WorldSize
             -- ^ Size of the big world
             -> Int
             -- ^ Pixel width (if 1, the small world will have the same size as the big one)
             -> Strategy
             -> IO [[CInt]]
             -- ^ the "small world"
mkSmallWorld s@(WorldSize (Coords heightEmptySpace widthEmptySpace)) multFactor strategy = do
  let nCols = quot widthEmptySpace $ fromIntegral multFactor
      nRows = quot heightEmptySpace $ fromIntegral multFactor
      mkRandomRow _ = take (fromIntegral nCols) <$> rands -- TODO use a Matrix directly
  smallMat <- mapM mkRandomRow [0..nRows-1]

  let mat = fromLists smallMat
      graph = graphOfIndex (mapMaterial Air) mat
  case strategy of
    StrictlyOneComponent -> case components graph of
      [_] -> return smallMat -- TODO return Matrix (mat) instead of list of list
      _   -> mkSmallWorld s multFactor strategy

graphOfIndex :: CInt -> Matrix CInt -> Graph
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
at mat (i, j) = getElem (succ i) (succ j) mat -- indexes start at 1 in Data.Matrix

connectedNeighbours :: CInt -> Coords -> Matrix CInt -> (Int, Int) -> [Coords]
connectedNeighbours matchIdx coords mat (nRows,nCols) =
  let neighbours = [translateInDir LEFT coords, translateInDir Down coords]
  in mapMaybe (\other@(Coords (Coord r) (Coord c)) ->
        if r < 0 || c < 0 || r >= nRows || c >= nCols || mat `at` (r, c) /= matchIdx
          then
            Nothing
          else
            Just other) neighbours

mkSpaceFromInnerMat :: WorldSize -> [[CInt]] -> Space
mkSpaceFromInnerMat s innerMatMaybeSmaller =
  let innerMat = extend s innerMatMaybeSmaller
      mat = fromLists $ addBorder s innerMat
  in Space mat s $ matToRenderGroups mat s

extend :: WorldSize -> [[a]] -> [[a]]
extend (WorldSize (Coords rs cs)) mat =
  extend' (fromIntegral rs) $ map (extend' $ fromIntegral cs) mat

extend' :: Int -> [a] -> [a]
extend' _ [] = error "extend empty list not supported"
extend' sz l@(_:_) =
  let len = length l
      addsTotal = sz - assert (len <= sz) len
      addsLeft = quot addsTotal 2
      addsRight = addsTotal - addsLeft
  in replicate addsLeft (head l) ++ l ++ replicate addsRight (last l)

rands :: IO [CInt]
rands = randomRsIO 0 1

addBorder :: WorldSize -> [[CInt]] -> [[CInt]]
addBorder (WorldSize (Coords _ widthEmptySpace)) l =
  let nCols = fromIntegral widthEmptySpace + 2 * borderSize
      wall = mapMaterial Wall
      wallRow = replicate nCols wall
      encloseIn b e = b ++ e ++ b
  in encloseIn (replicate borderSize wallRow) $ map (encloseIn $ replicate borderSize wall) l

borderSize :: Int
borderSize = 1

matToRenderGroups :: Matrix CInt -> WorldSize -> [RenderGroup]
matToRenderGroups mat s@(WorldSize (Coords _ cs)) =
  concat $
    forEachRowPure mat s $
      \row accessMaterial ->
          snd $ mapAccumL
                  (\col listMaterials@(material:_) ->
                     let count = length listMaterials
                         materialColor = case material of
                           Wall -> wallColors
                           Air -> airColors
                         materialChar = case material of
                           Wall -> 'Z'
                           Air -> ' '
                     in (col + fromIntegral count,
                         RenderGroup (Coords row col) materialColor materialChar count))
                  (Coord 0) $ group $ map accessMaterial [0..pred cs]

getInnerMaterial :: Coords -> Space -> Material
getInnerMaterial (Coords (Coord r) (Coord c)) (Space mat _ _) =
  mapInt $ mat `at` (r+borderSize, c+borderSize)


-- | @Coord 0 0@ corresponds to indexes 1 1 in matrix :
-- <https://hackage.haskell.org/package/matrix-0.3.5.0/docs/Data-Matrix.html#v:getElem indices start at 1>.
getMaterial :: Coords -> Space -> Material
getMaterial coords@(Coords r c) space@(Space _ (WorldSize (Coords rs cs)) _)
  | r < 0 || c < 0       = Wall
  | r > rs-1 || c > cs-1 = Wall
  | otherwise = getInnerMaterial coords space

materialToLocation :: Material -> Location
materialToLocation m = case m of
  Wall -> OutsideWorld
  Air  -> InsideWorld

location :: Coords -> Space -> Location
location c s = materialToLocation $ getMaterial c s

strictLocation :: Coords -> Space -> Location
strictLocation coords@(Coords r c) space@(Space _ (WorldSize (Coords rs cs)) _)
    | r < 0 || c < 0 || r > rs-1 || c > cs-1 = InsideWorld
    | otherwise = materialToLocation $ getInnerMaterial coords space


{-# INLINABLE renderSpace #-}
renderSpace :: (Draw e, MonadReader e m, MonadIO m)
            => Space
            -> Coords
            -> m Coords
renderSpace (Space _ _ renderedWorld) upperLeft = do
  let worldCoords = move borderSize Down $ move borderSize RIGHT upperLeft
  mapM_ (renderGroup worldCoords) renderedWorld
  return worldCoords

{-# INLINABLE renderGroup #-}
renderGroup :: (Draw e, MonadReader e m, MonadIO m)
            => Coords
            -> RenderGroup
            -> m ()
renderGroup worldCoords (RenderGroup pos colors char count) =
  drawChars count char (sumCoords pos worldCoords) colors

locationFunction :: Boundaries
                 -> Space
                 -> Maybe (Window Int)
                 -> Coords
                 -> (Coords -> Location)
locationFunction f space@(Space _ sz _) mayTermWindow wcc =
  let worldLocation = (`location` space)
      worldLocationExcludingBorders = (`strictLocation` space)
      terminalLocation (Window h w) coordsInWorld =
        let (Coords (Coord r) (Coord c)) = sumCoords coordsInWorld wcc
        in if r >= 0 && r < h && c >= 0 && c < w
             then
               InsideWorld
             else
               OutsideWorld
      productLocations l l' = case l of
        InsideWorld -> l'
        OutsideWorld -> OutsideWorld

  in case f of
    WorldFrame -> worldLocation
    TerminalWindow -> maybe
                        worldLocation
                        (\wd coo-> if contains coo sz then OutsideWorld else terminalLocation wd coo)
                        mayTermWindow
    Both       -> maybe
                    worldLocation
                    (\wd coo-> productLocations (terminalLocation wd coo) (worldLocationExcludingBorders coo))
                    mayTermWindow

{--

reboundMaxRecurse :: Space -> Int -> Coords -> Maybe Coords
reboundMaxRecurse sz maxRecurse (Coords r c) =
  let mayR = reboundIntMaxRecurse sz maxRecurse r
      mayC = reboundIntMaxRecurse sz maxRecurse c
  in  case mayR of
        Nothing -> Nothing
        (Just newR) -> case mayC of
            Nothing -> Nothing
            (Just newC) -> Just $ Coords (Row newR) (Col newC)

reboundIntMaxRecurse :: Space -> Int -> Int -> Maybe Int
reboundIntMaxRecurse s@(WorldSize sz) maxRecurse i
  | maxRecurse == 0 = Nothing
  | i < 0     = reboundIntMaxRecurse s rec $ -i
  | i > sz-1  = reboundIntMaxRecurse s rec $ 2*(sz-1)-i
  | otherwise = Just i
  where rec = pred maxRecurse

rebound :: Space -> Coords -> Coords
rebound sz (Coords r c) = Coords (reboundInt sz r) (reboundInt sz c)

reboundInt :: Space -> Int -> Int
reboundInt s@(WorldSize sz) i
  | i < 0     = reboundInt s $ -i
  | i > sz-1  = reboundInt s $ 2*(sz-1)-i
  | otherwise = i

--}
