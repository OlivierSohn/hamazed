{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Key.Types
    ( Key(..)
    ) where

import           Imj.Prelude
import           Imj.Geo.Discrete.Types( Direction(..) )

-- | Represents a key-press, read from stdin.
data Key = AlphaNum Char
         -- ^ An alphanumeric key
         | Arrow Direction
         -- ^ One of the four direction arrows
         | Escape
         -- ^ The escape key
         | Unknown
         -- ^ An unhandled key
