
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveGeneric #-}

module Geo.Continuous.Types
    ( Vec2(..)
    ) where

import           Imajuscule.Prelude

import           GHC.Generics( Generic )

data Vec2 = Vec2 Float Float deriving(Generic, Eq, Show)
