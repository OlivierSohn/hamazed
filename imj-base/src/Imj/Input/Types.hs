{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Input.Types
    ( Key(..)
    , PlayerInput(..)
    -- * reexports
    , MonadIO
    , module Imj.Timing
    ) where


import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Data.Int(Int64)

import           Imj.Geo.Discrete.Types(Direction(..))
import           Imj.Timing

-- | Represents a key-press, read from stdin.
data Key = AlphaNum Char
         -- ^ An alphanumeric key
         | Arrow Direction
         -- ^ One of the four direction arrows
         | Escape
         -- ^ The escape key
         | StopProgram
         -- ^ To be interpreted as "the program should stop now".
         | Unknown
         -- ^ An unhandled key
         deriving(Show)

class PlayerInput a where
  -- | Blocks until a 'Key' is produced.
  getKey :: (MonadIO m)
         => a
         -> m Key

  -- | Call this function to undo a getKey : it will fill a queue that is read
  -- before getting actual player input.
  unGetKey :: (MonadIO m)
           => a
           -> Key
           -> m ()

  getKeyBefore :: (MonadIO m)
               => a
               -> Time Point System
               -- ^ The time before which we should get the key.
               -> m (Maybe Key)
               -- ^ Nothing when the timeout was reached.

  tryGetKey :: (MonadIO m)
            => a
            -> m (Maybe Key)
            -- ^ Nothing when no input is available.

  someInputIsAvailable :: (MonadIO m)
                       => a
                       -> m Bool

  -- Return 'True' when the program should end
  programShouldEnd :: (MonadIO m)
                   => a -> m Bool
