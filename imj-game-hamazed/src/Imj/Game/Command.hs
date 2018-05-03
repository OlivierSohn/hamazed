{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Imj.Game.Command
      ( Command(..)
      , command
      , runClientCommand
      , maxOneSpace
      -- * utilities
      , onWorldInfosChanged
      ) where

import           Imj.Prelude

import           Data.Attoparsec.Text(Parser, takeText, endOfInput, char
                                    , peekChar, peekChar', skipSpace, takeWhile1)
import           Data.Char(isSpace, toLower, isAlphaNum)
import           Data.Text(pack, unsnoc)
import qualified Data.Text as Text

import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Types
import           Imj.Game.Hamazed.Types

import           Imj.Game.Draw
import           Imj.Game.Hamazed.Infos
import           Imj.Game.Hamazed.Color
import           Imj.Graphics.Text.ColorString
import           Imj.Graphics.UI.Animation
import           Imj.Graphics.UI.Chat
import           Imj.Graphics.UI.RectContainer
import           Imj.Timing

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
      onWorldInfosChanged
    AssignColor color -> do
      let n = maybe (ClientName "no name") getPlayerName p
      putPlayer sid $ Player n Present $ mkPlayerColors color
      onWorldInfosChanged
    Says what ->
      stateChat $ addMessage $ ChatMessage $
        name <> colored (":" <> what) chatMsgColor
    Leaves detail -> do
      maybe
        (return ())
        (\n -> putPlayer sid $ n { getPlayerStatus = Absent })
          p
      onWorldInfosChanged
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

{- Returns a parser of commands.
-}
command :: GameLogic g => Parser (Either Text (Command (ServerT g)))
command = do
  skipSpace
  peekChar' >>= \case -- we peek to issue an error on wrong commands (instead of interpreting them as a message)
    '/' -> do
      char '/' *> skipSpace
      cmdName <- Text.map toLower <$> takeWhile1 isAlphaNum
      skipSpace
      peekChar >>= maybe
        (return ())
        (\c -> if c == ':'
            then
              void $ char ':'
            else
              return ())
      skipSpace
      case cmdName of
        "name" -> Right . RequestApproval . AssignName . ClientName . maxOneSpace <$> takeText <* endOfInput
        _ -> cmdParser cmdName
    _ -> Right . RequestApproval . Says . maxOneSpace <$> (takeText <* endOfInput)


-- TODO replace by withWorldINfoChange, that animates the change
-- | This function should be called to force an update of displayed world informations.
{-# INLINABLE onWorldInfosChanged #-}
onWorldInfosChanged :: (GameLogic s, MonadState (AppState s) m)
                    => m ()
onWorldInfosChanged =
  gets game >>= \(Game _ screen g _ (UIAnimation (UIEvolutions j upDown _) p) _ names _ _ _ _) -> do
    let newLeft =
          let (horizontalDist, verticalDist) = computeViewDistances
              vp = getViewport To screen g
              (_, _, leftMiddle, _) = getSideCenters $ mkRectContainerAtDistance vp horizontalDist verticalDist
              (_, (_,infos)) = mkWorldInfos Normal From screen names g
          in mkTextAnimRightAligned leftMiddle leftMiddle infos 1 (fromSecs 1)
        newAnim = UIAnimation (UIEvolutions j upDown newLeft) p -- TODO use mkUIAnimation to have a smooth transition
    putAnimation newAnim
