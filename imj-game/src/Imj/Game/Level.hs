{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Imj.Game.Level
    ( LevelOutcome(..)
    ) where


import           Imj.Prelude

data LevelOutcome =
    Lost !Text
   -- ^ 'Text' is the reason why the 'Level' was lost.
  | Won
  deriving(Generic, Eq, Show)
instance Binary LevelOutcome
instance NFData LevelOutcome
