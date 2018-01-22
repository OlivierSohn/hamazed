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
  case k of
    Escape      -> return $ Just $ Interrupt Quit
    StopProgram -> return $ Just $ Interrupt Quit
    _ -> case intent of
      Configure -> return $ case k of
        AlphaNum ' ' -> Just $ StartGame
        AlphaNum c -> Just $ Configuration c
        _ -> Nothing
      Play ->
        getGameState >>= \(GameState _ _ _ _ _ (Level n _ finished) _ _) ->
          case finished of
            Nothing -> return $ case k of
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
            Just (LevelFinished stop _ ContinueMessage) -> return $ Just $
              case stop of
                Won -> if n < lastLevel
                         then
                           StartLevel (succ n)
                         else
                           EndGame
                (Lost _) -> StartLevel firstLevel
            _ -> return $ Nothing -- between level end and proposal to continue
