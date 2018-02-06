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
             => Key
             -> m (Maybe GenEvent)
eventFromKey k = do
  intent <- getUserIntent
  case k of
    Escape      -> return $ Just $ Evt $ Interrupt Quit
    StopProgram -> return $ Just $ Evt $ Interrupt Quit
    AlphaNum 'y' -> return $ Just $ Evt CycleRenderingOptions
    _ -> case intent of
      Configure -> return $ case k of
        AlphaNum ' ' -> Just $ Evt $ StartGame
        AlphaNum c -> Just $ Evt $ Configuration c
        _ -> Nothing
      Play ->
        getGameState >>= \(GameState _ _ _ _ _ (Level n _ finished) _ _) ->
          case finished of
            Nothing -> return $ case k of
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
              _ -> Nothing
            Just (LevelFinished stop _ ContinueMessage) -> return $ Just $ Evt $
              case stop of
                Won -> if n < lastLevel
                         then
                           StartLevel (succ n)
                         else
                           EndGame
                (Lost _) -> StartLevel firstLevel
            _ -> return $ Nothing -- between level end and proposal to continue
