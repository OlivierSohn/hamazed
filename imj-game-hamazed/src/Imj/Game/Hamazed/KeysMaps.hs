{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Game.Hamazed.KeysMaps
    ( eventFromKey
    ) where

import           Imj.Prelude

import           Imj.Game.Hamazed.Loop.Event.Types
import           Imj.Game.Hamazed.State.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.Types
import           Imj.Geo.Discrete.Types
import           Imj.Input.Types

-- | Maps a 'Key' (pressed by the player) to an 'Event'.
eventFromKey :: (MonadState AppState m)
             => Key
             -> m (Maybe GenEvent)
eventFromKey k = case k of -- TODO handle chat : when pressing Enter, swap between chatting and normal mode
  Escape      -> return $ Just $ Evt $ Interrupt Quit
  StopProgram -> return $ Just $ Evt $ Interrupt Quit
  Tab -> return $ Just $ Evt $ ChatCmd ToggleEditing
  _ -> getChatMode >>= \case
    Editing -> return $ case k of
      Delete     -> Just $ Evt $ ChatCmd DeleteAtEditingPosition
      BackSpace  -> Just $ Evt $ ChatCmd DeleteBeforeEditingPosition
      AlphaNum c -> Just $ Evt $ ChatCmd $ Insert c
      Arrow dir  -> Just $ Evt $ ChatCmd $ Navigate dir
      Enter      -> Just $ Evt SendChatMessage
      _ -> Nothing
    NotEditing -> case k of
      AlphaNum 'y' -> return $ Just $ Evt CycleRenderingOptions
      _ -> do
        (ClientState activity state) <- getClientState
        case activity of
          Over -> return Nothing
          Ongoing -> case state of
            Excluded -> return Nothing
            Setup -> return $ case k of
              AlphaNum ' ' -> Just $ CliEvt $ ExitedState Setup
              AlphaNum c   -> Just $ Evt $ Configuration c
              _ -> Nothing
            PlayLevel status -> case status of
              New -> return Nothing
              Running -> maybe
                (case k of
                  AlphaNum c -> case c of
                    'k' -> Just $ CliEvt $ Action Laser Down
                    'i' -> Just $ CliEvt $ Action Laser Up
                    'j' -> Just $ CliEvt $ Action Laser LEFT
                    'l' -> Just $ CliEvt $ Action Laser RIGHT
                    'd' -> Just $ CliEvt $ Action Ship Down
                    'e' -> Just $ CliEvt $ Action Ship Up
                    's' -> Just $ CliEvt $ Action Ship LEFT
                    'f' -> Just $ CliEvt $ Action Ship RIGHT
                    'r'-> Just $ Evt ToggleEventRecording
                    _   -> Nothing
                  _ -> Nothing)
                (\case
                  (LevelFinished stop _ ContinueMessage) -> Just $ Evt $ EndLevel stop
                  _ -> Nothing) -- between level end and proposal to continue
                  <$> getLevelStatus
              Paused _ -> return Nothing
              CancelledNoConnectedPlayer -> return Nothing
