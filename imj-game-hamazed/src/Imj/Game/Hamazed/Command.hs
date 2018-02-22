{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Game.Hamazed.Command
      ( Command(..)
      , command
      , runCommand
      , maxOneSpace
      ) where

import           Imj.Prelude

import           Data.Attoparsec.Text(Parser, takeText, endOfInput, string, char, peekChar', skipSpace, space)
import           Data.Char(isSpace)
import           Data.Text(pack, unsnoc)

import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.State.Types
import           Imj.Game.Hamazed.Types

import           Imj.Game.Hamazed.World.Ship
--import           Imj.Graphics.Color.Types

runCommand :: (MonadState AppState m)
           => ShipId
           -> Command
           -> m ()
runCommand sid (AssignName name) = do
  putPlayer sid (Player name Present)
  updateShipsText
runCommand _ (PutShipColor _) = undefined
runCommand sid (Says what) = getPlayer sid >>= \n ->
  stateChat $ addMessage $ ChatMessage $ getPlayerUIName n <> ":" <> what
runCommand sid (Leaves detail) = getPlayer sid >>= \n -> do
  maybe
    (return ())
    (\(Player a _) -> putPlayer sid $ Player a Absent)
      n
  updateShipsText
  stateChat $ addMessage $ ChatMessage $ getPlayerUIName n <> " " <>
    case detail of
      Intentional -> "leaves the game intentionally."
      ConnectionError t -> "leaves the game due to a connection error : " <> t

maxOneSpace :: Text -> Text
maxOneSpace t = go t False []
 where
  go txt prevSpace res =
    case unsnoc txt of
      Nothing -> pack res
      Just (rest, c) ->
        if isSpace c
          then
            go rest (not $ null res) res
          else
            go rest False $
              if prevSpace
                then c:' ':res
                else c:res


command :: Parser Command
command = do
  skipSpace
  peekChar' >>= \case
    '/' ->
      char '/' *> do
        skipSpace
        cmdType <- string "name" <|> string "color"
        void $ char ':' <|> space
        skipSpace
        case cmdType of
          "name" -> AssignName . PlayerName . maxOneSpace <$> takeText <* endOfInput
          "color" -> undefined
          _ -> error "logic"
    _ -> Says . maxOneSpace <$> (takeText <* endOfInput)
