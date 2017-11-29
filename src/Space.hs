{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Space
    ( Space(..)
    , renderSpace
    , renderIfNotColliding
    , WallType(..)
    , RandomParameters(..)
    , Strategy(..)
    , Material(..)
    , getMaterial
    , location
    , strictLocation
    , mkDeterministicallyFilledSpace
    , mkRandomlyFilledSpace
    , mkEmptySpace
    ) where

import           Imajuscule.Prelude

import           GHC.Generics( Generic )

import           Data.Graph( Graph
                           , graphFromEdges
                           , components )
import           Data.List(length, group, concat, mapAccumL)
import           Data.Maybe(mapMaybe)
import           Data.Vector(Vector, slice, (!))
import           Data.Matrix( getElem
                            , fromLists
                            , getMatrixAsVector
                            , Matrix
                            , nrows, ncols )

import           Foreign.C.Types( CInt(..) )

import           Color
import           Console
import           Geo.Types
import           Geo.Discrete( translateInDir )
import           Render
import           Util( replicateElements
                     , randomRsIO )
import           WorldSize( Location(..)
                          , WorldSize(..) )


data WallType = None
              | Deterministic
              | Random RandomParameters

data Strategy = StrictlyOneComponent

data RandomParameters = RandomParameters {
    _randomWallsBlockSize :: !Int
  , _randomWallsStrategy :: !Strategy
}

newtype RenderGroup = RenderGroup (Row, Col, (Color8Code, Color8Code), Char, Int)

data Space = Space {
    _space :: !(Matrix CInt)
  , _spaceSize :: !WorldSize -- ^ represents the aabb of the space without the border
  , _spaceRender :: ![RenderGroup]
}

data Material = Air
              | Wall
              deriving(Generic, Eq, Show)

forEachRowPure :: Matrix CInt -> WorldSize -> (Row -> (Col -> Material) -> b) -> [b]
forEachRowPure mat (WorldSize (Coords (Row rs) (Col cs))) f =
  let rows = [0..rs-1]
      colInternalLength = cs+2
      matAsOneVector = flatten mat -- this is O(1)
  in map (\r -> do
    let row = slice (1 + (r+1) * colInternalLength) cs matAsOneVector
    f (Row r) (\(Col c) -> mapInt $ row ! c)) rows

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
mkDeterministicallyFilledSpace s@(WorldSize (Coords (Row heightEmptySpace) (Col widthEmptySpace))) =
  let wall = mapMaterial Wall
      air  = mapMaterial Air

      middleRow = replicate widthEmptySpace air
      collisionRow = replicate 2 air ++ replicate (widthEmptySpace-4) wall ++ replicate 2 air
      ncolls = 8
      nEmpty = heightEmptySpace - ncolls
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
mkSmallWorld s@(WorldSize (Coords (Row heightEmptySpace) (Col widthEmptySpace))) multFactor strategy = do
  let nCols = quot widthEmptySpace multFactor
      nRows = quot heightEmptySpace multFactor
      mkRandomRow _ = take nCols <$> rands -- TODO use a Matrix directly
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
      coords = [Coords (Row r) (Col c) | c <-[0..nCols-1], r <- [0..nRows-1], mat `at` (r, c) == matchIdx]
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
  in mapMaybe (\other@(Coords (Row r) (Col c)) ->
        if r < 0 || c < 0 || r >= nRows || c >= nCols || mat `at` (r, c) /= matchIdx
          then
            Nothing
          else
            Just other) neighbours

mkSpaceFromInnerMat :: WorldSize -> [[CInt]] -> Space
mkSpaceFromInnerMat s innerMatMaybeSmaller =
  let innerMat = extend s innerMatMaybeSmaller
      mat = fromLists $ addBorder s innerMat
  in Space mat s $ render mat s

extend :: WorldSize -> [[a]] -> [[a]]
extend (WorldSize (Coords (Row rs) (Col cs))) mat =
  extend' rs $ map (extend' cs) mat

extend' :: Int -> [a] -> [a]
extend' _ [] = error "extend empty list not supported"
extend' sz l@(e:_) =
  let len = length l
      addsTotal = sz - assert (len <= sz) len
      addsLeft = quot addsTotal 2
      addsRight = addsTotal - addsLeft
  in replicate addsLeft e ++ l ++ replicate addsRight (last l)

rands :: IO [CInt]
rands = randomRsIO (0,1)

addBorder :: WorldSize -> [[CInt]] -> [[CInt]]
addBorder (WorldSize (Coords _ (Col widthEmptySpace))) l =
  let nCols = widthEmptySpace + 2 * borderSize
      wall = mapMaterial Wall
      wallRow = replicate nCols wall
      encloseIn b e = b ++ e ++ b
  in encloseIn (replicate borderSize wallRow) $ map (encloseIn $ replicate borderSize wall) l

borderSize :: Int
borderSize = 1

render :: Matrix CInt -> WorldSize -> [RenderGroup]
render mat s@(WorldSize (Coords _ (Col cs))) =
  concat $
    forEachRowPure mat s $
      \row accessMaterial ->
          snd $ mapAccumL
                  (\col@(Col c) listMaterials@(material:_) ->
                     let count = length listMaterials
                     in (Col (c+count),
                         RenderGroup
                           (row, col,
                            case material of
                              Wall -> wallColors
                              Air -> airColors,
                            case material of
                              Wall -> 'Z'
                              Air -> ' ',
                            count)))
                  (Col 0) $ group $ map (accessMaterial . Col) [0..cs-1]

getInnerMaterial :: Coords -> Space -> Material
getInnerMaterial (Coords (Row r) (Col c)) (Space mat _ _) =
  mapInt $ mat `at` (r+borderSize, c+borderSize)


-- | 0,0 Coord corresponds to 1,1 matrix
getMaterial :: Coords -> Space -> Material
getMaterial coords@(Coords (Row r) (Col c)) space@(Space _ (WorldSize (Coords (Row rs) (Col cs))) _)
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
strictLocation coords@(Coords (Row r) (Col c)) space@(Space _ (WorldSize (Coords (Row rs) (Col cs))) _)
    | r < 0 || c < 0 || r > rs-1 || c > cs-1 = InsideWorld
    | otherwise = materialToLocation $ getInnerMaterial coords space

renderSpace :: Space -> RenderState -> IO RenderState
renderSpace (Space _ (WorldSize (Coords (Row rs) (Col cs))) renderedWorld) upperLeft = do
  let horizontalWall = replicate (cs + 2)
      lowerLeft = move (rs+1) Down upperLeft

  fg <- setRawForeground worldFrameColor
  -- upper wall
  renderState <- renderStr (horizontalWall '_') upperLeft
  let worldCoords = go RIGHT renderState

  -- left & right walls
  let leftWallCoords = take rs $ iterate (go Down) renderState
      rightWallCoords = take rs $ iterate (go Down) $ move (cs+1) RIGHT renderState
  mapM_ (renderChar_ '|') (leftWallCoords ++ rightWallCoords)

  -- lower wall
  renderStr_ (horizontalWall 'T') lowerLeft
  restoreForeground fg

  -- world
  mapM_ (renderGroup worldCoords) renderedWorld
  return worldCoords


renderGroup :: RenderState -> RenderGroup -> IO ()
renderGroup worldCoords (RenderGroup (r, c, colors, char, count)) =
  renderColoredChars count char colors $ translate r c worldCoords


renderIfNotColliding :: Char -> Coords -> Space -> RenderState -> IO ()
renderIfNotColliding char worldCoords space r =
  case getMaterial worldCoords space of
    Air  -> renderChar char worldCoords r
    Wall -> return ()

{--

reboundMaxRecurse :: Space -> Int -> Coords -> Maybe Coords
reboundMaxRecurse sz maxRecurse (Coords (Row r) (Col c)) =
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
rebound sz (Coords (Row r) (Col c)) = Coords (Row $ reboundInt sz r) (Col $ reboundInt sz c)

reboundInt :: Space -> Int -> Int
reboundInt s@(WorldSize sz) i
  | i < 0     = reboundInt s $ -i
  | i > sz-1  = reboundInt s $ 2*(sz-1)-i
  | otherwise = i

--}
