{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Game.Hamazed.Loop.Event
    ( needsRendering
    , getKeyTime
    , getTimedEvent
    -- * Reexports
    , module Imj.Game.Hamazed.Loop.Event.Types
    ) where

import           Imj.Prelude

import           Imj.Game.Hamazed.Level
import           Imj.Game.Hamazed.Loop.Deadlines
import           Imj.Game.Hamazed.Loop.Event.Types
import           Imj.Game.Hamazed.Types
import           Imj.Timing

-- | Tells if after handling an 'Event' we should render or not.
needsRendering :: Event -> Bool
needsRendering = \case
  (Action Ship _) -> False -- When the ship accelerates, nothing changes visually
  _ -> True

getKeyTime :: Event -> Maybe KeyTime
getKeyTime (Timeout (Deadline k _)) = Just k
getKeyTime _                        = Nothing

getTimedEvent :: GameState -> IO TimestampedEvent
getTimedEvent state =
  getEvent state >>= \evt -> do
    t <- getSystemTime
    return $ TimestampedEvent evt t

getEvent :: GameState -> IO Event
getEvent state = do
  mayEvent <- getEvent' state
  case mayEvent of
    Just event -> return event
    Nothing -> getEvent state

getEvent' :: GameState -> IO (Maybe Event)
getEvent' state@(GameState _ _ _ _ level _) = do
  t <- getSystemTime
  let deadline = getNextDeadline state t
  getEventForMaybeDeadline level deadline t
