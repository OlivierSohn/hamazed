{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Game.Hamazed.Command
      ( Command(..)
      , command
      , runClientCommand
      , maxOneSpace
      ) where

import           Imj.Prelude

import           Data.Attoparsec.Text(Parser, takeText, decimal, endOfInput, string, char
                                    , peekChar', skipSpace, space)
import           Data.Char(isSpace)
import           Data.Text(pack, unsnoc)

import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.State.Types
import           Imj.Game.Hamazed.Types
import           Imj.Graphics.Color.Types

import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.World.Ship
import           Imj.Graphics.Text.ColorString

runClientCommand :: (MonadState AppState m)
                 => ShipId
                 -> ClientCommand
                 -> m ()
runClientCommand sid cmd = getPlayer sid >>= \p -> do
  let name = getPlayerUIName' p
  case cmd of
    AssignName name' -> do
      let colors = maybe (mkPlayerColors refShipColor) getPlayerColors p
      putPlayer sid $ Player name' Present colors
      updateShipsText
    AssignColor color -> do
      let n = maybe (PlayerName "no name") getPlayerName p
      putPlayer sid $ Player n Present $ mkPlayerColors color
      updateShipsText
    Says what ->
      stateChat $ addMessage $ ChatMessage $
        name <> colored (":" <> what) chatMsgColor
    Leaves detail -> do
      maybe
        (return ())
        (\n -> putPlayer sid $ n { getPlayerStatus = Absent })
          p
      updateShipsText
      stateChat $ addMessage $ ChatMessage $
        name <> colored (" " <>
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
        reportColorScheme <|> do
          cmdType <- string "name" <|> string "color"
          void $ char ':' <|> space
          skipSpace
          case cmdType of
            "name" -> Right . ClientCmd . AssignName . PlayerName . maxOneSpace <$> takeText <* endOfInput
            "color" -> setColorScheme
            _ -> error "logic"
       where
        reportColorScheme =
          do void $ string "color" <* endOfInput
             return $ Right $ ServerRep $ Get ColorSchemeCenterKey
        setColorScheme = do
          skipSpace
          r <- decimal
          skipSpace
          g <- decimal
          skipSpace
          b <- decimal
          return $ ServerCmd . Put . ColorSchemeCenter <$> userRgb r g b
    _ -> Right . ClientCmd . Says . maxOneSpace <$> (takeText <* endOfInput)
