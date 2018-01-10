{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Game.Hamazed.State
      (
      -- * AppState type
        AppState(..)
      , RecordMode(..)
      , OccurencesHist
      , toColorStr
      -- * Create
      , createState
      -- * Access
      , getRenderable
      , getGameState
      , getGame
      -- * Modify
      , putGame
      , putGameState
      , onEvent
      , toggleRecordEvent
      , addIgnoredOverdues
      -- * reexports
      , MonadState
      ) where

import           Imj.Prelude
import           Control.Monad.State.Class(MonadState)
import           Control.Monad.State(state, get, put)
import           Data.Text(pack)

import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.Loop.Event.Types
import           Imj.Graphics.Class.Words
import           Imj.Graphics.Color
import           Imj.Graphics.Text.ColorString


data Occurences a = Occurences {
    _occurencesCount :: !Int
  , _occurencesItem :: !EventRepr
}

data AppState  = AppState {
    _appStateGame :: !Game
  , _appStateEventHistory :: !OccurencesHist
  -- ^ Can record which events where handled.
  , _appStateRecordEvents :: !RecordMode
  -- ^ Should the handled events be recorded?
}

data RecordMode = Record
                | DontRecord
                deriving(Eq)

data OccurencesHist = OccurencesHist {
    _occurencesHistList :: ![Occurences EventRepr]
  , _occurencesHistTailStr :: !ColorString
}

data EventRepr = Laser'
               | Ship'
               | MoveFlyingItems'
               | AnimateParticleSystems'
               | DisplayContinueMessage'
               | AnimateUI'
               | StartLevel'
               | EndGame'
               | Interrupt'
               | ToggleEventRecording'
               | IgnoredOverdue
               -- ^ Represents when an overdue deadline was ignored because its priority was lower
               -- than another overdue deadline.
               deriving(Eq, Show)

representation :: Event -> EventRepr
representation (StartLevel _)   = StartLevel'
representation EndGame          = EndGame'
representation (Interrupt _)    = Interrupt'
representation (Action Laser _) = Laser'
representation (Action Ship _)  = Ship'
representation (Timeout (Deadline _ MoveFlyingItems))        = MoveFlyingItems'
representation (Timeout (Deadline _ AnimateParticleSystems)) = AnimateParticleSystems'
representation (Timeout (Deadline _ DisplayContinueMessage)) = DisplayContinueMessage'
representation (Timeout (Deadline _ AnimateUI))              = AnimateUI'
representation ToggleEventRecording = ToggleEventRecording'

reprToCS :: EventRepr -> ColorString
reprToCS IgnoredOverdue = colored "X" red
reprToCS StartLevel' = colored "l" cyan
reprToCS EndGame'    = colored "E" cyan
reprToCS Interrupt'  = colored "I" yellow
reprToCS Laser'      = colored "L" cyan
reprToCS Ship'       = colored "S" blue
reprToCS MoveFlyingItems'        = colored "M" green
reprToCS AnimateParticleSystems' = colored "P" blue
reprToCS DisplayContinueMessage' = colored "C" white
reprToCS AnimateUI'              = colored "U" magenta
reprToCS ToggleEventRecording'   = colored "T" yellow


onEvent :: MonadState AppState m
        => Event -> m ()
onEvent evt = do
  when (evt == ToggleEventRecording) $ state toggleRecordEvent
  getRecording >>= \case
    Record -> state (addEvent evt)
    DontRecord -> return ()

getRenderable :: MonadState AppState m
              => m (GameState, [ColorString])
getRenderable =
  get >>= \(AppState (Game _ _ gameState) h r) -> do
    let strs = case r of
              Record -> toColorStr h `multiLine` 150 -- TODO screen width should be dynamic
              DontRecord -> []
    return (gameState, strs)

getRecording :: MonadState AppState m
             => m RecordMode
getRecording = do
  (AppState _ _ record) <- get
  return record

addEvent :: Event -> AppState -> ((), AppState)
addEvent e (AppState g es r) =
  let es' = addEventRepr (representation e) es
  in ((), AppState g es' r)

toggleRecordEvent :: AppState -> ((), AppState)
toggleRecordEvent (AppState g _ r) =
  let r' = case r of
        Record -> DontRecord
        DontRecord -> Record
  in ((), AppState g mkEmptyOccurencesHist r')

putGame :: MonadState AppState m => Game -> m ()
putGame g =
  get >>= \(AppState _ r h) ->
    put $ AppState g r h

putGameState :: MonadState AppState m => GameState -> m ()
putGameState s =
  get >>= \(AppState (Game i params _) r h) ->
    put $ AppState (Game i params s) r h

getGameState :: MonadState AppState m => m GameState
getGameState =
  getGame >>= \(Game _ _ s) -> return s

getGame :: MonadState AppState m => m Game
getGame =
  get >>= \(AppState g _ _) -> return g

addIgnoredOverdues :: MonadState AppState m
                   => Int -> m ()
addIgnoredOverdues n =
  get >>= \(AppState a hist record) -> do
    let hist' = case record of
          Record -> iterate (addEventRepr IgnoredOverdue) hist !! n
          DontRecord -> hist
    put $ AppState a hist' record

toColorStr :: OccurencesHist -> ColorString
toColorStr (OccurencesHist []    tailStr) = tailStr
toColorStr (OccurencesHist (x:_) tailStr) = tailStr <> toColorStr' x

addEventRepr :: EventRepr -> OccurencesHist -> OccurencesHist
addEventRepr e oh@(OccurencesHist h r) =
  case h of
    [] -> mkOccurencesHist (Occurences 1 e)
    Occurences n o:xs -> if o == e
                            then
                              OccurencesHist (Occurences (succ n) o:xs) r
                            else
                              let prevTailStr = toColorStr oh
                              in OccurencesHist (Occurences 1 e:h) prevTailStr

createState :: Game -> AppState
createState game =
  AppState game mkEmptyOccurencesHist DontRecord

toColorStr' :: Occurences EventRepr -> ColorString
toColorStr' (Occurences n e) =
  let r@(ColorString l) = reprToCS e
      col = case l of
        [] -> white
        ((_,LayeredColor _ fg):_) -> fg
  in r <> colored (pack $ replicate (pred n) '.' ) col

mkOccurencesHist :: Occurences EventRepr -> OccurencesHist
mkOccurencesHist o =
  OccurencesHist [o] mempty

mkEmptyOccurencesHist :: OccurencesHist
mkEmptyOccurencesHist = OccurencesHist [] mempty
