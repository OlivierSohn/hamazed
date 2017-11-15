{-# LANGUAGE DeriveGeneric #-}

module WorldSize
    ( WorldSize(..)
    , Location(..)
    ) where

import           GHC.Generics( Generic )

import           Geo( Coords(..)
                    , Col(..)
                    , Row(..) )

newtype WorldSize = WorldSize { _worldSizeValue :: Int } deriving(Generic, Eq, Show)

data Location = InsideWorld
              | OutsideWorld
              deriving(Eq, Show)
