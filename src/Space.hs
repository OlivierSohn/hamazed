{-# LANGUAGE DeriveGeneric #-}

module Space
    ( Space(..)
    , Material(..)
    , getMaterial
    , forEachRow
    , location
    , mkRectangle
    ) where


import           Data.Vector.Storable( slice )
import           GHC.Generics( Generic )

import           Numeric.LinearAlgebra.Data( (!)
                                           , fromLists
                                           , flatten
                                           , Matrix )

import           Foreign.C.Types( CInt(..) )

import           Geo( Coords(..)
                    , Col(..)
                    , Row(..) )
import           WorldSize( Location(..)
                          , WorldSize(..) )

data Space = Space {
    _space :: !(Matrix CInt)
  , _spaceSize :: !WorldSize -- ^ represents the aabb of the Air (TODO support non square aabbs)
  , _spaceRender :: ![String]
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

-- | creates an empty rectangle of size specified in parameters, with a one-element border
mkRectangle :: Row -> Col -> Space
mkRectangle (Row heightEmptySpace) (Col widthEmptySpace) =
  let ncols = widthEmptySpace + 2

      wall = mapMaterial Wall
      air  = mapMaterial Air

      upperRow = replicate ncols wall
      middleRow = wall : replicate widthEmptySpace air ++ [wall]
      collisionRow = wall : replicate 2 air ++ replicate (ncols-6) wall ++ replicate 2 air ++ [wall]
      ncolls = 8
      nEmpty = heightEmptySpace - ncolls
      n1 = quot nEmpty 2
      n2 = nEmpty - n1
      l = [upperRow] ++ replicate n1 middleRow ++ replicate ncolls collisionRow ++ replicate n2 middleRow ++ [upperRow]
      mat = fromLists l
      size = WorldSize $ Coords (Row heightEmptySpace) (Col widthEmptySpace)
  in Space mat size $ render mat size


render :: Matrix CInt -> WorldSize -> [String]
render mat s@(WorldSize (Coords _ (Col cs))) =
  forEachRowPure mat s (\accessMaterial -> map (\c -> case accessMaterial (Col c) of
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
