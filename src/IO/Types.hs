
{-# LANGUAGE NoImplicitPrelude #-}

module IO.Types
    ( Key(..)
    ) where


import           Geo(Direction(..))

data Key = Arrow Direction
         | Escape
         | Unknown
