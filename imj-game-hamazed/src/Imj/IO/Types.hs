{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.IO.Types
    ( Key(..)
    ) where

import           Imj.Prelude
import           Imj.Geo.Types( Direction(..) )

-- | A key-press read from stdin.
data Key = AlphaNum Char
         -- ^ An alphanumeric key
         | Arrow Direction
         -- ^ One of the four direction arrows
         | Escape
         -- ^ The escape key
         | Unknown
         -- ^ An unhandled key
