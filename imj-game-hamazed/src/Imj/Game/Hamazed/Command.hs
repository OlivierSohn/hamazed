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

import           Data.Attoparsec.Text(Parser, takeText, decimal, endOfInput, string, char, peekChar', skipSpace, space)
import           Data.Char(isSpace)
import           Data.Text(pack, unsnoc)

import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.State.Types
import           Imj.Game.Hamazed.Types

import           Imj.Game.Hamazed.World.Ship
import           Imj.Graphics.Text.ColorString
import           Imj.Graphics.Color.Types

runCommand :: (MonadState AppState m)
           => ShipId
           -> Command
           -> m ()
runCommand sid (AssignName name) = getPlayer sid >>= \p -> do
  let color = maybe chatMsgColor getPlayerColor p
  putPlayer sid $ Player name Present color
  updateShipsText
runCommand sid (AssignColor color) = getPlayer sid >>= \p -> do
  let name = maybe (PlayerName "no name") getPlayerName p
  putPlayer sid $ Player name Present color
  updateShipsText
runCommand sid (Says what) = getPlayer sid >>= \n ->
  stateChat $ addMessage $ ChatMessage $ getPlayerUIName n <> colored (":" <> what) chatMsgColor
runCommand sid (Leaves detail) = getPlayer sid >>= \n -> do
  maybe
    (return ())
    (\p -> putPlayer sid $ p { getPlayerStatus = Absent })
      n
  updateShipsText
  stateChat $ addMessage $ ChatMessage $ getPlayerUIName n
    <> colored (" " <>
    case detail of
      Intentional -> "leaves the game intentionally."
      ConnectionError t -> "leaves the game due to a connection error : " <> t) chatMsgColor

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


command :: Parser (Either Text Command)
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
          "name" -> Right . AssignName . PlayerName . maxOneSpace <$> takeText <* endOfInput
          "color" -> do
            skipSpace
            r <- decimal
            skipSpace
            g <- decimal
            skipSpace
            b <- decimal
            return $ AssignColor <$> userRgb r g b
          _ -> error "logic"
    _ -> Right . Says . maxOneSpace <$> (takeText <* endOfInput)
