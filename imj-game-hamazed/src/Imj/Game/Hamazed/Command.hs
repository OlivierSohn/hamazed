{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Game.Hamazed.Command
      ( Command(..)
      , command
      , runCommand
      ) where

import           Imj.Prelude

import           Data.Attoparsec.Text(Parser, takeText, endOfInput, string, char, peekChar', skipSpace, space)
import           Data.Map.Strict((!?))

import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.State.Types
import           Imj.Game.Hamazed.Types

import           Imj.Game.Hamazed.World.Ship
--import           Imj.Graphics.Color.Types

runCommand :: (MonadState AppState m)
           => ShipId
           -> Command
           -> m ()
runCommand sid (PutPlayerName name) = putPlayerName sid name >> updateShipsText
runCommand _ (PutShipColor _) = undefined
runCommand sid (Says what) = getPlayerNames >>= \names ->
  stateChat $ addMessage $ ChatMessage $ maybe "?" (\(PlayerName x) -> x) (names !? sid) <> ":" <> what

command :: Parser Command
command = do
  skipSpace
  peekChar' >>= \case
    '/' -> do
      char '/' *> do
        skipSpace
        cmdType <- string "name" <|> string "color"
        void $ char ':' <|> space
        skipSpace
        case cmdType of
          "name" -> PutPlayerName . PlayerName <$> takeText <* endOfInput
          "color" -> undefined
          _ -> error "logic"
    _ -> Says <$> (takeText <* endOfInput)
