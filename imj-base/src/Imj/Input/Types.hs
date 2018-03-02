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

-- | Represents the key pressed by a player.
data Key = AlphaNum Char
         -- ^ An alphanumeric key (includes punctuation characters)
         | Arrow Direction
         -- ^ One of the four direction arrows
         | Enter
         -- ^ The Enter key
         | Escape
         -- ^ The escape key
         | Tab
         -- ^ The tab key
         | BackSpace
         -- ^ Remove char LEFT of the edit point
         | Delete
         -- ^ Remove char at the edit point
         | StopProgram
         -- ^ To be interpreted as "the program should stop now".
         | Unknown
         -- ^ An unhandled key
         deriving(Show)

data FeedType =
    AutomaticFeed
    -- ^ An auxiliary thread feeds the queue.
  | ManualFeed
  -- ^ The queue needs to be "manually" fed by calling 'pollKeys' or 'waitKeys'

class PlayerInput a where

  -- | Return 'True' when the program should end
  programShouldEnd :: (MonadIO m)
                   => a -> m Bool

  keysQueue :: a -> TQueue Key

  queueType :: a -> FeedType
  -- | Use only if 'queueType' returns ManualFeed.
  pollKeys :: a -> IO ()
  -- | Use only if 'queueType' returns ManualFeed.
  waitKeysTimeout :: a -> Time Duration System -> IO ()
