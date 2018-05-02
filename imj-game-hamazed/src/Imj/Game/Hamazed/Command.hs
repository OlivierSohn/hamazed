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

import           Data.Attoparsec.Text(Parser, takeText, endOfInput, string, char
                                    , peekChar', skipSpace, space)
import           Data.Char(isSpace)
import           Data.Text(pack, unsnoc)

import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.State.Types
import           Imj.Game.Hamazed.Types

import           Imj.Game.Hamazed.Color
import           Imj.Graphics.Text.ColorString

runClientCommand :: (GameLogic g, MonadState (AppState g) m)
                 => ShipId
                 -> ClientCommand
                 -> m ()
runClientCommand sid cmd = getPlayer sid >>= \p -> do
  let name = getPlayerUIName' p
  case cmd of
    AssignName name' -> do
      let colors = maybe (mkPlayerColors refShipColor) getPlayerColors p
      putPlayer sid $ Player name' Present colors
      onPlayersChanged
    AssignColor color -> do
      let n = maybe (ClientName "no name") getPlayerName p
      putPlayer sid $ Player n Present $ mkPlayerColors color
      onPlayersChanged
    Says what ->
      stateChat $ addMessage $ ChatMessage $
        name <> colored (":" <> what) chatMsgColor
    Leaves detail -> do
      maybe
        (return ())
        (\n -> putPlayer sid $ n { getPlayerStatus = Absent })
          p
      onPlayersChanged
      stateChat $ addMessage $ ChatMessage $
        name <>
        colored
          ((<>) " leaves the game " $
          either
            ((<>) "due to a connection error : ")
            (const "intentionally.")
            detail)
          chatMsgColor


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


command :: GameLogic g => Parser (Either Text (Command (ServerT g)))
command =
  defaultCommand <|> commandParser

 where

  defaultCommand = skipSpace *> peekChar' >>= \case
    '/' -> setName
    _   -> speak

  speak = Right . ClientCmd . Says . maxOneSpace <$> (takeText <* endOfInput)

  setName = do
    char '/' *> skipSpace
    void $ string "name"
    (skipSpace *> void (char ':')) <|>
      (space *> skipSpace) -- NOTE the order matters
    skipSpace
    Right . ClientCmd . AssignName . ClientName . maxOneSpace <$> takeText <* endOfInput
