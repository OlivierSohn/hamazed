{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Game.Hamazed.KeysMaps
    ( eventFromKey
    ) where

import           Imj.Prelude

import           Imj.Game.Hamazed.Loop.Event.Types
import           Imj.Game.Hamazed.State.Types
import           Imj.Game.Hamazed.Types
import           Imj.Geo.Discrete.Types
import           Imj.Input.Types


-- | Maps a 'Key' (pressed by the player) to an 'Event'.
eventFromKey :: (MonadState AppState m)
             => Key -> m (Maybe Event)
eventFromKey k = do
  intent <- getUserIntent
  return $ case k of
    Escape      -> Just $ Interrupt Quit
    StopProgram -> Just $ Interrupt Quit
    _ -> case intent of
      Configure -> case k of
        AlphaNum ' ' -> Just $ StartLevel firstLevel
        _ -> Nothing
      Play -> case k of
        AlphaNum c -> case c of
          'k' -> Just $ Action Laser Down
          'i' -> Just $ Action Laser Up
          'j' -> Just $ Action Laser LEFT
          'l' -> Just $ Action Laser RIGHT
          'd' -> Just $ Action Ship Down
          'e' -> Just $ Action Ship Up
          's' -> Just $ Action Ship LEFT
          'f' -> Just $ Action Ship RIGHT
          'r'-> Just ToggleEventRecording
          _   -> Nothing
        _ -> Nothing
