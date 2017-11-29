
{-# LANGUAGE NoImplicitPrelude #-}

module IO.Types
    ( Key(..)
    ) where


import           Geo.Types( Direction(..) )

data Key = Arrow Direction
         | Escape
         | Unknown
