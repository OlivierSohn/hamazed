{-# OPTIONS_HADDOCK prune, hide #-} -- hide to avoid link in the doc to a module with no doc

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Geo.Types
    ( Direction(..)
    ) where

import           Imj.Prelude

-- | Discrete directions.
data Direction = Up | Down | LEFT | RIGHT deriving (Eq, Show)
