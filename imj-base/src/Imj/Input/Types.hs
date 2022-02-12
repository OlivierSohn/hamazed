{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Input.Types
    ( PlatformEvent(..)
    , Key(..)
    , PlayerInput(..)
    , FeedType(..)
    -- * reexports
    , MonadIO
    , TQueue
    , module Imj.Timing
    ) where


import           Imj.Prelude

import           Control.Concurrent.STM(TQueue)

import qualified Graphics.UI.GLFW as GLFW

import           Imj.Geo.Discrete.Types(Direction(..))
import           Imj.Timing
import           Imj.Log

data PlatformEvent =
    InterpretedKey !Key
    -- ^ Can correspond to multiple key presses, because it includes character events
    -- as described <http://www.glfw.org/docs/latest/input_guide.html#input_key here>.
  | StatefullKey !GLFW.Key !GLFW.KeyState !GLFW.ModifierKeys
  -- ^ Note that the same key press can generate both a 'InterpretedKey' and 'StatefullKey'
  | Message !MessageLevel !Text
  | StopProgram
  | FramebufferSizeChanges

-- | Represents the key pressed by a player.
data Key = AlphaNum !Char
         -- ^ An alphanumeric key (includes punctuation characters)
         | Arrow !Direction
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

  plaformQueue :: a -> TQueue PlatformEvent

  queueType :: a -> FeedType

  -- | Use 'pollKeys' only if 'queueType' == 'ManualFeed'.
  pollKeys :: a -> IO ()
  -- | Use 'waitKeys' only if 'queueType' == 'ManualFeed'.
  waitKeys :: a -> IO ()
  -- | Use 'stopWaitKeys' only if 'queueType' == 'ManualFeed'.
  stopWaitKeys :: a -> IO ()
  -- | Use 'waitKeysTimeout' only if 'queueType' == 'ManualFeed'.
  waitKeysTimeout :: a -> Time Duration System -> IO ()
