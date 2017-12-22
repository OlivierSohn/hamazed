
{-# LANGUAGE NoImplicitPrelude #-}

module Imj.IO.Types
    ( Key(..)
    ) where


import           Imj.Geo.Types( Direction(..) )

data Key = Arrow Direction
         | Escape
         | Unknown
