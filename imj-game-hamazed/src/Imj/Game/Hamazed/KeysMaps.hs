{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Game.Hamazed.KeysMaps
    ( eventFromKey
    ) where

import           Imj.Prelude

import           Imj.Game.Hamazed.Loop.Event.Types
import           Imj.Geo.Discrete.Types
import           Imj.Input.Types


-- | Maps a 'Key' (pressed by the player) to an 'Event'.
eventFromKey :: Key -> Maybe Event
eventFromKey = \case
  Escape -> Just $ Interrupt Quit
  AlphaNum c -> case c of
    'k' -> Just $ Action Laser Down
    'i' -> Just $ Action Laser Up
    'j' -> Just $ Action Laser LEFT
    'l' -> Just $ Action Laser RIGHT
    'd' -> Just $ Action Ship Down
    'e' -> Just $ Action Ship Up
    's' -> Just $ Action Ship LEFT
    'f' -> Just $ Action Ship RIGHT
    _   -> Nothing
  _ -> Nothing
