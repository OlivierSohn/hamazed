{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Input.Types
    ( Key(..)
    , PlayerInput(..)
    , FeedType(..)
    -- * reexports
    , MonadIO
    , TQueue
    , module Imj.Timing
    ) where


import           Imj.Prelude

import           Control.Concurrent.STM(TQueue)

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

data FeedType =
    AutomaticFeed
    -- ^ An auxiliary thread feeds the queue.
  | PollOrWaitOnEvents
  -- ^The queue needs to be "manually" fed by calling 'pollKeys' or 'waitKeys'

class PlayerInput a where

  -- | Return 'True' when the program should end
  programShouldEnd :: (MonadIO m)
                   => a -> m Bool

  keysQueue :: a -> TQueue Key

  queueType :: a -> FeedType
  -- | Use only if 'queueType' returns PollOrWaitOnEvents.
  pollKeys :: a -> IO ()
  -- | Use only if 'queueType' returns PollOrWaitOnEvents.
  waitKeysTimeout :: a -> Time Duration System -> IO ()
