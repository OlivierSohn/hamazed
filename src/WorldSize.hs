{-# LANGUAGE DeriveGeneric #-}

module WorldSize
    ( WorldSize(..)
    , location
    , Location(..)
    , rebound
    , reboundMaxRecurse
    ) where

import           GHC.Generics( Generic )

import           Geo( Coords(..)
                    , Col(..)
                    , Row(..) )

newtype WorldSize = WorldSize { _worldSizeValue :: Int } deriving(Generic, Eq, Show)

data Location = InsideWorld
              | OutsideWorld
              deriving(Eq, Show)

location :: Coords -> WorldSize -> Location
location (Coords (Row r) (Col c)) (WorldSize worldSize)
  | inside r && inside c = InsideWorld
  | otherwise            = OutsideWorld
  where inside x = x >= 0 && x < worldSize

reboundMaxRecurse :: WorldSize -> Int -> Coords -> Maybe Coords
reboundMaxRecurse sz maxRecurse (Coords (Row r) (Col c)) =
  let mayR = reboundIntMaxRecurse sz maxRecurse r
      mayC = reboundIntMaxRecurse sz maxRecurse c
  in  case mayR of
        Nothing -> Nothing
        (Just newR) -> case mayC of
            Nothing -> Nothing
            (Just newC) -> Just $ Coords (Row newR) (Col newC)

reboundIntMaxRecurse :: WorldSize -> Int -> Int -> Maybe Int
reboundIntMaxRecurse s@(WorldSize sz) maxRecurse i
  | maxRecurse == 0 = Nothing
  | i < 0     = reboundIntMaxRecurse s rec $ -i
  | i > sz-1  = reboundIntMaxRecurse s rec $ 2*(sz-1)-i
  | otherwise = Just i
  where rec = pred maxRecurse

rebound :: WorldSize -> Coords -> Coords
rebound sz (Coords (Row r) (Col c)) = Coords (Row $ reboundInt sz r) (Col $ reboundInt sz c)

reboundInt :: WorldSize -> Int -> Int
reboundInt s@(WorldSize sz) i
  | i < 0     = reboundInt s $ -i
  | i > sz-1  = reboundInt s $ 2*(sz-1)-i
  | otherwise = i
