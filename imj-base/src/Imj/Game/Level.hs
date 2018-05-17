{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Imj.Game.Level
    ( LevelOutcome(..)
    , LevelNumber(..)
    ) where


import           Imj.Prelude
import           Data.Aeson(ToJSON(..), FromJSON(..), ToJSONKey(..), FromJSONKey(..))

newtype LevelNumber = LevelNumber Int
  deriving(Generic, Show, NFData, Binary, Integral, Ord, Eq, Real, Enum, Num)
instance ToJSON LevelNumber
instance FromJSON LevelNumber
instance ToJSONKey LevelNumber
instance FromJSONKey LevelNumber

data LevelOutcome =
    Lost !Text
   -- ^ 'Text' is the reason why the 'Level' was lost.
  | Won
  deriving(Generic, Eq, Show)
instance Binary LevelOutcome
instance NFData LevelOutcome
