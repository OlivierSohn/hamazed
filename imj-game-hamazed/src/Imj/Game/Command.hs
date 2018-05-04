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
      , withAnim
      , withGameInfoAnimationIf
      , mkAnim
      ) where

import           Imj.Prelude

import           Control.Monad.IO.Class(MonadIO)
import           Data.Attoparsec.Text(Parser, takeText, endOfInput, char
                                    , peekChar, peekChar', skipSpace, takeWhile1)
import           Data.Char(isSpace, toLower, isAlphaNum)
import           Data.Text(pack, unsnoc)
import qualified Data.Text as Text

import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Types
import           Imj.Game.Hamazed.Types

import           Imj.ClientView.Types
import           Imj.Game.Draw
import           Imj.Game.Hamazed.Infos
import           Imj.Game.Hamazed.Color
import           Imj.Graphics.Text.ColorString
import           Imj.Graphics.Screen
import           Imj.Graphics.UI.Animation
import           Imj.Graphics.UI.Chat
import           Imj.Timing

runClientCommand :: (GameLogic g
                   , MonadState (AppState g) m, MonadIO m)
                 => ShipId
                 -> ClientCommand
                 -> m ()
runClientCommand sid cmd = getPlayer sid >>= \p -> do
  let name = getPlayerUIName' p
  case cmd of
    AssignName name' ->
      withAnim Normal (pure ()) $
        putPlayer sid $ Player name' Present $ maybe (mkPlayerColors refShipColor) getPlayerColors p
    AssignColor color ->
      withAnim Normal (pure ()) $
        putPlayer sid $ Player (maybe (ClientName "") getPlayerName p) Present $ mkPlayerColors color
    Says what ->
      stateChat $ addMessage $ ChatMessage $
        name <> colored (":" <> what) chatMsgColor
    Leaves detail -> do
      maybe
        (return ())
        (\n ->
            withAnim Normal (pure ()) $
              putPlayer sid $ n { getPlayerStatus = Absent })
          p
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

withGameInfoAnimationIf :: (MonadState (AppState g) m
                          , MonadIO m
                          , GameLogic g)
                        => Bool
                        -> m a
                        -> m a
withGameInfoAnimationIf condition act =
  f act
 where
  f = bool id (withAnim Normal (pure ())) condition

withAnim :: (MonadState (AppState g) m
                        , MonadIO m
                        , GameLogic g)
                      => InfoType
                      -> m ()
                      -- ^ This action will be run at the end of the animation.
                      -> m a
                      -> m a
withAnim it finalize act = do
  gets game >>= \(Game _ _ (GameState g1 _) _ _ names1 _ _ _ _) -> do
    res <- act
    gets game >>= \(Game _ screen (GameState g2 _) _ _ names2 _ _ _ _) -> do
      t <- liftIO getSystemTime
      putAnimation =<< mkAnim it t screen names1 names2 g1 g2 finalize
    return res

mkAnim :: (Monad m, GameLogic g1, GameLogic g2)
       => InfoType
       -> Time Point System
       -> Screen
       -> Map ClientId Player
       -- ^ from
       -> Map ClientId Player
       -- ^ to
       -> Maybe g1
       -- ^ from
       -> Maybe g2
       -- ^ to
       -> m ()
       -- ^ action to run when the animation is over
       -> m UIAnimation
mkAnim it t screen namesI namesF gI gF finalizer = do
  let (hDist, vDist) = computeViewDistances
      from = mkWorldInfos Normal From screen namesI gI
      to   = mkWorldInfos it     To   screen namesF gF
      anim = mkUIAnimation from to hDist vDist t
  when (isNothing $ _deadline $ getProgress $ anim) finalizer
  return anim
