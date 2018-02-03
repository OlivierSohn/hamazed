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
             => Key -> m (Maybe (Either Event ClientEvent))
eventFromKey k = do
  intent <- getUserIntent
  case k of
    Escape      -> return $ Just $ Left $ Interrupt Quit
    StopProgram -> return $ Just $ Left $ Interrupt Quit
    AlphaNum 'y' -> return $ Just $ Left CycleRenderingOptions
    _ -> case intent of
      Configure -> return $ case k of
        AlphaNum ' ' -> Just $ Left $ StartGame
        AlphaNum c -> Just $ Left $ Configuration c
        _ -> Nothing
      Play ->
        getGameState >>= \(GameState _ _ _ _ _ (Level n _ finished) _ _) ->
          case finished of
            Nothing -> return $ case k of
              AlphaNum c -> case c of
                'k' -> Just $ Right $ Action Laser Down
                'i' -> Just $ Right $ Action Laser Up
                'j' -> Just $ Right $ Action Laser LEFT
                'l' -> Just $ Right $ Action Laser RIGHT
                'd' -> Just $ Right $ Action Ship Down
                'e' -> Just $ Right $ Action Ship Up
                's' -> Just $ Right $ Action Ship LEFT
                'f' -> Just $ Right $ Action Ship RIGHT
                'r'-> Just $ Left ToggleEventRecording
                _   -> Nothing
              _ -> Nothing
            Just (LevelFinished stop _ ContinueMessage) -> return $ Just $ Left $
              case stop of
                Won -> if n < lastLevel
                         then
                           StartLevel (succ n)
                         else
                           EndGame
                (Lost _) -> StartLevel firstLevel
            _ -> return $ Nothing -- between level end and proposal to continue
