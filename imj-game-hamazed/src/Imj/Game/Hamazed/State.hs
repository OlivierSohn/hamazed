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
      -- * Modify
      , setGame
      , onEvent
      , toggleRecordEvent
      ) where

import           Imj.Prelude
import           Control.Monad.State.Class(MonadState)
import           Control.Monad.State(state, get)
import           Data.Text(pack)

import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.Loop.Event
import           Imj.Graphics.Class.Words
import           Imj.Graphics.Color
import           Imj.Graphics.Text.ColorString

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
reprToCS StartLevel' = colored "l" cyan
reprToCS EndGame'    = colored "E" cyan
reprToCS Interrupt'  = colored "I" yellow
reprToCS Laser'      = colored "L" red
reprToCS Ship'       = colored "S" blue
reprToCS MoveFlyingItems'        = colored "M" green
reprToCS AnimateParticleSystems' = colored "P" blue
reprToCS DisplayContinueMessage' = colored "C" white
reprToCS AnimateUI'              = colored "U" magenta
reprToCS ToggleEventRecording'   = colored "T" yellow


onEvent :: MonadState AppState m
        => Event -> m [ColorString]
onEvent evt = do
  when (evt == ToggleEventRecording) $ state toggleRecordEvent
  getRecording >>= \case
    -- TODO 150 (screen width) should be part of the state
    Record -> state (addEvent evt) >>= \history ->
      return $ toColorStr history `multiLine` 150
    DontRecord -> return []

getRecording :: MonadState AppState m
             => m RecordMode
getRecording = do
  (AppState _ _ record) <- get
  return record

addEvent :: Event -> AppState -> (OccurencesHist, AppState)
addEvent e (AppState g es r) =
  let es' = addEvent' e es
  in (es', AppState g es' r)

toggleRecordEvent :: AppState -> ((), AppState)
toggleRecordEvent (AppState g _ r) =
  let r' = case r of
        Record -> DontRecord
        DontRecord -> Record
  in ((), AppState g mkEmptyOccurencesHist r')

setGame :: Maybe Game -> AppState -> ((), AppState)
setGame g (AppState _ es r) = ((), AppState g es r)

toColorStr :: OccurencesHist -> ColorString
toColorStr (OccurencesHist []    tailStr) = tailStr
toColorStr (OccurencesHist (x:_) tailStr) = tailStr <> toColorStr' x

addEvent' :: Event -> OccurencesHist -> OccurencesHist
addEvent' e' oh@(OccurencesHist h r) =
  let e = representation e'
  in case h of
    [] -> mkOccurencesHist (Occurences 1 e)
    Occurences n o:xs -> if o == e
                            then
                              OccurencesHist (Occurences (succ n) o:xs) r
                            else
                              let prevTailStr = toColorStr oh
                              in OccurencesHist (Occurences 1 e:h) prevTailStr

createState :: AppState
createState = AppState Nothing mkEmptyOccurencesHist DontRecord

data Occurences a = Occurences {
    _occurencesCount :: !Int
  , _occurencesItem :: !EventRepr
}

data AppState  = AppState {
    _appStateGame :: !(Maybe Game)
  , _appStateEventHistory :: !OccurencesHist
  , _appStateRecordEvents :: !RecordMode
}

data RecordMode = Record
                | DontRecord
                deriving(Eq)

data OccurencesHist = OccurencesHist {
    _occurencesHistList :: ![Occurences EventRepr]
  , _occurencesHistTailStr :: !ColorString
}

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
