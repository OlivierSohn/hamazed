{-# LANGUAGE NoImplicitPrelude #-}

module Render.Backends.Delta
                          ( preferredBuffering
                          -- reexports
                          , module Render.Backends.Internal.Delta
                          , BufferMode(..)
                          ) where

import           Imajuscule.Prelude

import           System.IO( BufferMode(..) )

import           Geo.Discrete.Types

import           Render.Backends.Internal.Delta

preferredBuffering :: BufferMode
preferredBuffering = BlockBuffering $ Just (maxBound :: Int)
