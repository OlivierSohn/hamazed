{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Game.Hamazed.Level
    ( drawLevelMessage
    , drawLevelState
    , messageDeadline
    ) where

import           Imj.Prelude

import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.Loop.Event.Priorities
import           Imj.Game.Hamazed.Loop.Event.Types
import           Imj.Geo.Discrete
import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.Render
import           Imj.Graphics.Text.Alignment
import           Imj.Graphics.UI.Colored
import           Imj.Timing

messageDeadline :: Level -> Maybe Deadline
messageDeadline (Level _ _ mayLevelFinished) =
  maybe Nothing
  (\(LevelFinished _ timeFinished messageType) ->
    case messageType of
      InfoMessage ->
        let nextMessageStep = addDuration (fromSecs 2) timeFinished
        in  Just $ Deadline nextMessageStep continueMsgPriority DisplayContinueMessage
      ContinueMessage -> Nothing)
  mayLevelFinished

{-# INLINABLE drawLevelState #-}
drawLevelState :: (Draw e, MonadReader e m, MonadIO m)
               => Coords Pos
               -> Int
               -> LevelFinished
               -> m ()
drawLevelState centerRef level (LevelFinished stop _ messageState) = do
  let stopMsg = case stop of
        (Lost reason) -> "You Lose (" <> reason <> ")"
        Won           -> "You Win!"
  drawAligned_ (Colored (messageColor stop) stopMsg) (mkCentered centerRef)
  when (messageState == ContinueMessage) $
    drawAligned_ (Colored neutralMessageColor $
      if level == lastLevel
        then
          "You reached the end of the game!"
        else
          let action = case stop of
                            (Lost _) -> "restart"
                            Won      -> "continue"
          in "Press a key to " <> action <> " ..." :: Text)
           (mkCentered $ move 2 Down centerRef)


{-# INLINABLE drawLevelMessage #-}
drawLevelMessage :: (Draw e, MonadReader e m, MonadIO m)
                 => Level
                 -> Coords Pos
                 -> m ()
drawLevelMessage (Level level _ levelState) ref =
  mapM_ (drawLevelState ref level) levelState
