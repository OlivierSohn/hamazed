
{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Geo.Continuous.Types
    ( Vec2(..)
    ) where

import           Imj.Prelude

data Vec2 = Vec2 !Float !Float deriving(Eq, Show, Ord)
