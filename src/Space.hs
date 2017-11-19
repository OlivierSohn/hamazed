{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Space
    ( Space(..)
    , renderSpace
    , renderIfNotColliding
    , Material(..)
    , getMaterial
    , forEachRow
    , location
    , mkDeterministicallyFilledSpace
    , mkRandomlyFilledSpace
    ) where

import           Imajuscule.Prelude

import           Control.Exception( assert )

import           GHC.Generics( Generic )

import           Data.Graph( Graph
                           , graphFromEdges
                           , components )
import           Data.Text( Text, pack )
import           Data.List(length)
import           Data.Maybe(mapMaybe)
import           Data.Vector.Storable( slice )

import           Numeric.LinearAlgebra.Data( (!)
                                           , fromLists
                                           , flatten
                                           , Matrix
                                           , size )

import           Foreign.C.Types( CInt(..) )

import           Console( renderChar_
                        , renderStr
                        , renderStr_
                        , renderText_ )
import           Geo( Coords(..)
                    , Col(..)
                    , Direction(..)
                    , Row(..)
                    , translateInDir )
import           Render( RenderState
                       , RenderState
                       , go
                       , move
                       , renderChar)
import           Util( replicateElements
                     , randomRsIO )
import           WorldSize( Location(..)
                          , WorldSize(..) )

data Space = Space {
    _space :: !(Matrix CInt)
  , _spaceSize :: !WorldSize -- ^ represents the aabb of the space without the border
  , _spaceRender :: ![Text]
}

data Material = Air
              | Wall
              deriving(Generic, Eq, Show)

forEachRow :: (Monad m) => Space -> (Row -> (Col -> Material) -> m ()) -> m ()
forEachRow (Space mat (WorldSize (Coords (Row rs) (Col cs))) _) f = do
  let rows = [0..rs-1]
      colInternalLength = cs+2
      matAsOneVector = flatten mat -- this is O(1)
  mapM_ (\r -> do
    let row = slice (1 + (r+1) * colInternalLength) cs matAsOneVector
    f (Row r) (\(Col c) -> mapInt $ row ! c)) rows

forEachRowPure :: Matrix CInt -> WorldSize -> ((Col -> Material) -> b) -> [b]
forEachRowPure mat (WorldSize (Coords (Row rs) (Col cs))) f =
  let rows = [0..rs-1]
      colInternalLength = cs+2
      matAsOneVector = flatten mat -- this is O(1)
  in map (\r -> do
    let row = slice (1 + (r+1) * colInternalLength) cs matAsOneVector
    f (\(Col c) -> mapInt $ row ! c)) rows

-- unfortunately I didn't find a Matrix implementation that supports arbitrary types
-- so I need to map my type on a CInt
mapMaterial :: Material -> CInt
mapMaterial Air  = 0
mapMaterial Wall = 1

mapInt :: CInt -> Material
mapInt 0 = Air
mapInt 1 = Wall
mapInt _ = error "mapInt arg out of bounds"

mkDeterministicallyFilledSpace :: WorldSize -> IO Space
mkDeterministicallyFilledSpace s@(WorldSize (Coords (Row heightEmptySpace) (Col widthEmptySpace))) = do
  let wall = mapMaterial Wall
      air  = mapMaterial Air

      middleRow = replicate widthEmptySpace air
      collisionRow = replicate 2 air ++ replicate (widthEmptySpace-4) wall ++ replicate 2 air
      ncolls = 8
      nEmpty = heightEmptySpace - ncolls
      n1 = quot nEmpty 2
      n2 = nEmpty - n1
      l = replicate n1 middleRow ++ replicate ncolls collisionRow ++ replicate n2 middleRow
  return $ mkSpaceFromInnerMat s l

-- | creates a rectangle of size specified in parameters, with a one-element border.
--  it uses IO for random numbers
mkRandomlyFilledSpace :: WorldSize -> IO Space
mkRandomlyFilledSpace s = do
  let multFactor = 6 -- using 4 it once took a very long time (one minute, then I killed the process)
                     -- 6 has always been ok
  smallWorldMat <- mkSmallWorld s multFactor

  let innerMat = replicateElements multFactor $ map (replicateElements multFactor) smallWorldMat
  return $ mkSpaceFromInnerMat s innerMat

-- | This function generates a random world that has a single "Air" connected component
--   (so that the ship can access every Air point in the level).
--  It might take a long time (TODO compute compexity) especially if worldsize is big
--  and multFactor is small.
mkSmallWorld :: WorldSize
             -- ^ Size of the big world
             -> Int
             -- ^ Pixel width (if 1, the small world will have the same size as the big one)
             -> IO [[CInt]]
             -- ^ the "small world"
mkSmallWorld s@(WorldSize (Coords (Row heightEmptySpace) (Col widthEmptySpace))) multFactor = do
  let ncols = quot widthEmptySpace multFactor
      nrows = quot heightEmptySpace multFactor
      mkRandomRow _ = take ncols <$> rands -- TODO use a Matrix directly
  smallMat <- mapM mkRandomRow [0..nrows-1]

  let mat = fromLists smallMat
      graph = graphOfIndex (mapMaterial Air) mat
  case components graph of
    [_] -> return smallMat -- TODO return Matrix (mat) instead of list of list
    _   -> mkSmallWorld s multFactor

graphOfIndex :: CInt -> Matrix CInt -> Graph
graphOfIndex matchIdx mat =
  let sz@(nrows,ncols) = size mat
      coords = [Coords (Row r) (Col c) | c <-[0..ncols-1], r <- [0..nrows-1], mat ! r ! c == matchIdx]
      edges = map (\c -> (c, c, connectedNeighbours matchIdx c mat sz)) coords
      (graph, _, _) = graphFromEdges edges
  in graph

connectedNeighbours :: CInt -> Coords -> Matrix CInt -> (Int, Int) -> [Coords]
connectedNeighbours matchIdx coords mat (nrows,ncols) =
  let neighbours = [translateInDir LEFT coords, translateInDir Down coords]
  in mapMaybe (\other@(Coords (Row r) (Col c)) ->
        if r < 0 || c < 0 || r >= nrows || c >= ncols || mat !r !c /= matchIdx
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
  in replicate (sz - assert (len <= sz) len) e ++ l

rands :: IO [CInt]
rands = randomRsIO (0,1)

addBorder :: WorldSize -> [[CInt]] -> [[CInt]]
addBorder (WorldSize (Coords _ (Col widthEmptySpace))) l =
  let ncols = widthEmptySpace + 2 * borderSize
      wall = mapMaterial Wall
      wallRow = replicate ncols wall
      encloseIn b e = b ++ e ++ b
  in encloseIn (replicate borderSize wallRow) $ map (encloseIn $ replicate borderSize wall) l

borderSize :: Int
borderSize = 1

render :: Matrix CInt -> WorldSize -> [Text]
render mat s@(WorldSize (Coords _ (Col cs))) =
  map pack $ forEachRowPure mat s (\accessMaterial -> map (\c -> case accessMaterial (Col c) of
    Wall -> 'Z'
    Air -> ' ') [0..cs-1])

-- | 0,0 Coord corresponds to 1,1 matrix
getMaterial :: Coords -> Space -> Material
getMaterial (Coords (Row r) (Col c)) (Space mat (WorldSize (Coords (Row rs) (Col cs))) _)
  | r < 0 || c < 0 = Wall
  | r > rs-1 || c > cs-1 = Wall
  | otherwise = mapInt $ mat !(r+1) !(c+1)

location :: Coords -> Space -> Location
location c s = case getMaterial c s of
  Wall -> OutsideWorld
  Air  -> InsideWorld


renderSpace :: Space -> RenderState -> IO RenderState
renderSpace (Space _ (WorldSize (Coords (Row rs) (Col cs))) renderedWorld) upperLeft = do
  let horizontalWall = replicate (cs + 2)
      lowerLeft = move (rs+1) Down upperLeft

  -- upper wall
  renderState <- renderStr (horizontalWall '_') upperLeft
  let worldCoords = go RIGHT renderState

  -- left & right walls
  let leftWallCoords = take rs $ iterate (go Down) renderState
      rightWallCoords = take rs $ iterate (go Down) $ move (cs+1) RIGHT renderState
  mapM_ (renderChar_ '|') (leftWallCoords ++ rightWallCoords)

  -- lower wall
  renderStr_ (horizontalWall 'T') lowerLeft

  -- world
  mapM_ (\(r, txt) -> renderText_ txt (move r Down worldCoords)) $ zip [0..] renderedWorld

  return worldCoords

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
