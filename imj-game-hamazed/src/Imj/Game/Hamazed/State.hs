{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Game.Hamazed.State
      (
      -- * Create
        createState
      -- * Modify
      , onEvent
      , toggleRecordEvent
      , addIgnoredOverdues
      -- * reexports
      , module Imj.Game.Hamazed.State.Types
      ) where

import           Imj.Prelude
import           Prelude(putStr, putStrLn, length)

import           Control.Monad.State.Class(MonadState)
import           Control.Monad.State(state, get, put)
import           Control.Monad.Reader.Class(MonadReader)
import           Control.Monad.IO.Class(MonadIO)

import           Data.Text(pack)

import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.Loop.Create
import           Imj.Game.Hamazed.Loop.Draw
import           Imj.Game.Hamazed.Loop.Event
import           Imj.Game.Hamazed.Loop.Update
import           Imj.Game.Hamazed.Parameters
import           Imj.Game.Hamazed.Server
import           Imj.Game.Hamazed.State.Types
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.Class.Words hiding (length)
import           Imj.Graphics.Color
import           Imj.Graphics.Render.FromMonadReader
import           Imj.Graphics.Text.ColorString
import           Imj.Graphics.UI.RectContainer
import           Imj.Timing

representation :: UpdateEvent -> EventRepr
representation (Left (ServerEvent e)) = case e of
  (LaserShot _ _) -> Laser'
  (PeriodicMotion _) -> PeriodicMotion'
representation (Right e) = case e of
  CycleRenderingOptions -> CycleRenderingOptions'
  (Configuration _) -> Configuration'
  EndGame -> EndGame'
  StartGame -> StartGame'
  (StartLevel _) -> StartLevel'
  (Interrupt _) -> Interrupt'
  (Timeout (Deadline _ _ (AnimateParticleSystem _))) -> AnimateParticleSystem'
  (Timeout (Deadline _ _ DisplayContinueMessage)) -> DisplayContinueMessage'
  (Timeout (Deadline _ _ AnimateUI)) -> AnimateUI'
  ToggleEventRecording -> ToggleEventRecording'


reprToCS :: EventRepr -> ColorString
reprToCS IgnoredOverdue = colored "X" red
reprToCS StartLevel' = colored "l" cyan
reprToCS EndGame'    = colored "E" cyan
reprToCS StartGame'  = colored "S" cyan
reprToCS Configuration' = colored "C" yellow
reprToCS CycleRenderingOptions' = colored "R" yellow
reprToCS Interrupt'  = colored "I" yellow
reprToCS Laser'      = colored "L" cyan
reprToCS PeriodicMotion' = colored "S" blue
reprToCS MoveFlyingItems'        = colored "M" green
reprToCS AnimateParticleSystem' = colored "P" blue
reprToCS DisplayContinueMessage' = colored "C" white
reprToCS AnimateUI'              = colored "U" magenta
reprToCS ToggleEventRecording'   = colored "T" yellow

{-# INLINABLE onEvent #-}
onEvent :: (MonadState AppState m, MonadReader e m, ClientNode e, Render e, MonadIO m)
        => Maybe GenEvent -> m ()
onEvent Nothing = handleEvent Nothing -- if a rendergroup exists, render and reset the group
onEvent (Just (CliEvt clientEvt)) = send clientEvt
onEvent (Just (Evt ToggleEventRecording)) = state toggleRecordEvent
onEvent (Just (Evt    evt)) = onUpdateEvent $ Right evt
onEvent (Just (SrvEvt evt)) = onUpdateEvent $ Left evt

{-# INLINABLE onUpdateEvent #-}
onUpdateEvent :: (MonadState AppState m, MonadReader e m, Render e, MonadIO m)
              => UpdateEvent -> m ()
onUpdateEvent e = do
  getRecording >>= \case
    Record -> state $ addEvent e
    DontRecord -> return ()
  handleEvent $ Just e

{-# INLINABLE handleEvent #-}
handleEvent :: (MonadState AppState m, MonadReader e m, Render e, MonadIO m)
            => Maybe UpdateEvent -> m ()
handleEvent e = do
  addToCurrentGroupOrRenderAndStartNewGroup e
  maybe
    (return ())
    (\evt -> do
      t1 <- liftIO getSystemTime
      updateAppState evt
      t2 <- liftIO getSystemTime
      addUpdateTime $ t1...t2)
    e

{-# INLINE addUpdateTime #-}
addUpdateTime :: MonadState AppState m
              => Time Duration System -> m ()
addUpdateTime add =
  get >>= \(AppState t a (EventGroup d e prevT f) b c g de) ->
    put $ AppState t a (EventGroup d e (add |+| prevT) f) b c g de

{-# INLINABLE addToCurrentGroupOrRenderAndStartNewGroup #-}
addToCurrentGroupOrRenderAndStartNewGroup :: (MonadState AppState m, MonadReader e m, Render e, MonadIO m)
                                          => Maybe UpdateEvent -> m ()
addToCurrentGroupOrRenderAndStartNewGroup evt = do
  get >>= \(AppState prevTime game prevGroup b c d e) -> do
    let onRender = do
          debug >>= \case
            True -> liftIO $ putStr $ groupStats prevGroup
            False -> return ()
          renderAll
          liftIO (tryGrow evt mkEmptyGroup) >>= maybe
            (error "growing an empty group never fails")
            return
    liftIO (tryGrow evt prevGroup) >>= maybe
      (onRender >>= \group -> liftIO getSystemTime >>= \curTime -> return (curTime, group))
      (return . (,) prevTime)
    >>= \(t,g) -> put $ AppState t game g b c d e


groupStats :: EventGroup -> String
groupStats (EventGroup l _ t _) =
  replicate (pred $ length l) ' ' ++ "|" ++
    replicate (10 - (length l)) ' ' ++ " u " ++ showTime t

{-# INLINABLE renderAll #-}
renderAll :: (MonadState AppState m, MonadReader e m, Render e, MonadIO m)
          => m ()
renderAll = do
  t1 <- liftIO getSystemTime
  draw
  getUserIntent >>= \case
    Configure -> do
      (Screen _ center) <- getCurScreen
      getGameState >>= \(GameState _ _ (World _ _ (Space _ sz _) _) _ _ _ _ _) ->
        draw' $ mkRectContainerWithCenterAndInnerSize center sz
    _ -> return ()
  t2 <- liftIO getSystemTime
  getEvtStrs >>= zipWithM_ (\i evtStr -> drawAt evtStr $ Coords i 0) [0..]
  (dtDelta, dtCmds, dtFlush) <- renderToScreen
  debug >>= \case
    True -> liftIO $ putStrLn $ " d " ++ showTime (t1...t2)
                              ++ " de " ++ showTime dtDelta
                              ++ " cmd " ++ showTime dtCmds
                              ++ " fl " ++ showTime dtFlush
    False -> return ()

{-# INLINABLE getEvtStrs #-}
getEvtStrs :: MonadState AppState m
              => m [ColorString]
getEvtStrs =
  get >>= \(AppState _ _ _ h r _ _) ->
    return $ case r of
      Record -> toColorStr h `multiLine` 150 -- TODO screen width should be dynamic
      DontRecord -> []

{-# INLINABLE getRecording #-}
getRecording :: MonadState AppState m
             => m RecordMode
getRecording = do
  (AppState _ _ _ _ record _ _) <- get
  return record

addEvent :: UpdateEvent -> AppState -> ((), AppState)
addEvent e (AppState t g evts es r b d) =
  let es' = addEventRepr (representation e) es
  in ((), AppState t g evts es' r b d)

toggleRecordEvent :: AppState -> ((), AppState)
toggleRecordEvent (AppState t g e _ r b d) =
  let r' = case r of
        Record -> DontRecord
        DontRecord -> Record
  in ((), AppState t g e mkEmptyOccurencesHist r' b d)

addIgnoredOverdues :: MonadState AppState m
                   => Int -> m ()
addIgnoredOverdues n =
  get >>= \(AppState t a e hist record b d) -> do
    let hist' = iterate (addEventRepr IgnoredOverdue) hist !! n
    put $ AppState t a e hist' record b d

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

createState :: Maybe Size -> Bool -> IO AppState
createState ms dbg = do
  sid <- mkShipId
  game <- initialGame ms [sid]
  t <- getSystemTime
  return $ AppState t game mkEmptyGroup mkEmptyOccurencesHist DontRecord (ParticleSystemKey 0) dbg

{-# INLINABLE debug #-}
debug :: MonadState AppState m => m Bool
debug =
  get >>= \(AppState _ _ _ _ _ _ d) -> return d

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
