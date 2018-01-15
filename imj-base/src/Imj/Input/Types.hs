{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Input.Types
    ( Key(..)
    , PlayerInput(..)
    -- * reexports
    , SystemTime
    , MonadIO
    ) where


import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)

import           Imj.Geo.Discrete.Types( Direction(..) )
import           Imj.Timing

-- | Represents a key-press, read from stdin.
data Key = AlphaNum Char
         -- ^ An alphanumeric key
         | Arrow Direction
         -- ^ One of the four direction arrows
         | Escape
         -- ^ The escape key
         | Unknown
         -- ^ An unhandled key
         deriving(Show)

class PlayerInput a where
  -- | Blocks until a 'Key' is produced.
  getKey :: (MonadIO m)
         => a
         -> m Key

  getKeyTimeout :: (MonadIO m)
                => a
                -> SystemTime
                -- ^ Current time measured by the caller.
                -> Int
                -- ^ A timeout in microseconds.
                -> m (Maybe Key)
                -- ^ Nothing when the timeout was reached.

  tryGetKey :: (MonadIO m)
            => a
            -> m (Maybe Key)
            -- ^ Nothing when no input is available.
