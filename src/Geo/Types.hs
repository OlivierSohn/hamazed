{-# LANGUAGE NoImplicitPrelude #-}

module Geo.Types
    ( Direction(..)
    ) where

import           Imajuscule.Prelude

data Direction = Up | Down | LEFT | RIGHT deriving (Eq, Show)
