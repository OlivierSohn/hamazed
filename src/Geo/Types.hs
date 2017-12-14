{-# OPTIONS_HADDOCK prune, hide #-} -- hide to avoid link in the doc to a module with no doc

{-# LANGUAGE NoImplicitPrelude #-}

module Geo.Types
    ( Direction(..)
    ) where

import           Imajuscule.Prelude

data Direction = Up | Down | LEFT | RIGHT deriving (Eq, Show)
