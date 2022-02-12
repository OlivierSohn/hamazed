{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Imj.Game.State
      (
      -- * Create
        createState
      -- * Modify
      , onEvent
      , addIgnoredOverdues
      , useInstrument
      -- * reexports
      , module Imj.Game.Class
      ) where

import           Imj.Prelude
import           Prelude(putStr, putStrLn, length)

import           Control.Monad.State.Strict(get, gets, modify', state)
import           Control.Monad.Reader.Class(asks)
import           Data.Text(pack)

import           Imj.Categorized
import           Imj.Control.Concurrent.AsyncGroups.Class
import           Imj.Geo.Discrete.Types
import           Imj.Event
import           Imj.Game.Class
import           Imj.Game.Configuration
import           Imj.Graphics.Screen
import           Imj.Input.Types
import           Imj.Server.Class
import           Imj.Server.Types
import           Imj.ServerView.Types

import           Imj.Audio.Midi
import           Imj.Game.Audio.Class
import           Imj.Game.Draw
import           Imj.Game.Status
import           Imj.Game.Update
import           Imj.Graphics.Class.HasSizedFace
import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.Class.Words hiding (length)
import           Imj.Graphics.Color
import           Imj.Graphics.Render.FromMonadReader
import           Imj.Graphics.Text.ColorString hiding(putStrLn, putStr)
import           Imj.Graphics.Text.Render
import           Imj.Graphics.UI.Animation.Types
import           Imj.Graphics.UI.Chat
import           Imj.Input.FromMonadReader
import           Imj.Music.Instrument
import           Imj.Music.Instruments

{-# INLINABLE onEvent #-}
onEvent :: (GameLogicT e ~ g, s ~ ServerT g
          , DrawGroupMember (ServerEventT s)
          , DrawGroupMember (ClientOnlyEvtT g)
          , ServerCmdParser s
          , StateValueT (ServerT g) ~ GameStateValue
          , MonadState (AppState g) m
          , MonadReader e m, Client e, Render e, PlayerInput e, HasSizedFace e, AsyncGroups e, Audio e
          , MonadIO m)
        => Maybe (GenEvent g)
        -> m ()
onEvent mayEvt = do
  checkPlayerEndsProgram
  gets appStateDebug >>= \case
    (Debug True) -> liftIO $ putStrLn $ show mayEvt -- TODO make this more configurable (use levels 1, 2 of debugging)
    (Debug False) -> return ()
  onEvent' mayEvt
 where
  checkPlayerEndsProgram =
    playerEndsProgram >>= \end ->
      when end $ do
        -- Note that it is safe to send this several times
        asks sendToServer' >>= \f -> f $ OnCommand $ RequestApproval $ Leaves $ Right ()

{-# INLINABLE onEvent' #-}
onEvent' :: (GameLogicT e ~ g, s ~ ServerT g
           , DrawGroupMember (ServerEventT s)
           , DrawGroupMember (ClientOnlyEvtT g)
           , ServerCmdParser s
           , StateValueT (ServerT g) ~ GameStateValue
           , MonadState (AppState g) m
           , MonadReader e m, Client e, Render e, HasSizedFace e, AsyncGroups e, Audio e
           , MonadIO m)
         => Maybe (GenEvent g) -> m ()
onEvent' = maybe (handleEvent Nothing) -- if a rendergroup exists, render and reset the group
  (\case
    CliEvt e -> asks sendToServer' >>= \f -> f e
    Evt    evt -> onUpdate $ Right evt
    SrvEvt evt -> onUpdate $ Left evt)
 where
  onUpdate e@(Right ToggleEventRecording) =
    handleEvent $ Just e
  onUpdate e = do
    gets appStateRecordEvents >>= \case
      Record -> modify' $ addEvent e
      DontRecord -> return ()
    handleEvent $ Just e

{-# INLINABLE handleEvent #-}
handleEvent :: (GameLogicT e ~ g, s ~ ServerT g
              , DrawGroupMember (ClientOnlyEvtT g)
              , DrawGroupMember (ServerEventT s)
              , ServerCmdParser s
              , StateValueT s ~ GameStateValue
              , MonadState (AppState g) m
              , MonadReader e m, Client e, Render e, HasSizedFace e, AsyncGroups e, Audio e
              , MonadIO m)
            => Maybe (UpdateEvent g) -> m ()
handleEvent e = do
  addToCurrentGroupOrRenderAndStartNewGroup e
  maybe
    (return ())
    (\evt -> do
      t1 <- liftIO getSystemTime
      updateAppState evt
      t2 <- liftIO getSystemTime
      addUpdateTime $!! t1...t2)
    e

{-# INLINE addUpdateTime #-}
addUpdateTime :: MonadState (AppState g) m
              => Time Duration System -> m ()
addUpdateTime add =
  modify' $
    \s@(AppState _ _ _ e@(EventGroup _ _ prevT _) _ _ _ _ _ _) ->
      s { eventsGroup = e { evtGroupUpdateDuration = add |+| prevT} }

{-# INLINABLE addToCurrentGroupOrRenderAndStartNewGroup #-}
addToCurrentGroupOrRenderAndStartNewGroup :: (GameLogicT e ~ g
                                            , DrawGroupMember (ServerEventT (ServerT g))
                                            , DrawGroupMember (ClientOnlyEvtT g)
                                            , MonadState (AppState g) m
                                            , MonadReader e m, Render e, Client e
                                            , MonadIO m)
                                          => Maybe (UpdateEvent g) -> m ()
addToCurrentGroupOrRenderAndStartNewGroup evt =
  get >>= \(AppState prevTime _ _ prevGroup _ _ _ _ _ _) -> do
    let onRender = do
          gets appStateDebug >>= \case
            (Debug True) -> liftIO $ putStr $ groupStats prevGroup
            (Debug False) -> return ()
          renderAll
          liftIO (tryGrow evt mkEmptyGroup) >>= maybe
            (error "growing an empty group never fails")
            return
    liftIO (tryGrow evt prevGroup) >>= maybe
      (do
        group <- onRender
        curTime <- liftIO getSystemTime
        return $ curTime `deepseq` (curTime, group))
      (return . (,) prevTime) >>= \(t,g) ->
        modify' (\s -> s { timeAfterRender = t, eventsGroup = g} )


groupStats :: EventGroup s -> String
groupStats (EventGroup l _ t _) =
  replicate (pred $ length l) ' ' ++
  "|" ++
  replicate (10 - length l) ' ' ++
  " u " ++
  showTime t

{-# INLINABLE renderAll #-}
renderAll :: (GameLogicT e ~ g
            , MonadState (AppState g) m
            , MonadReader e m, Render e, Client e
            , MonadIO m)
          => m ()
renderAll = do
  t1 <- liftIO getSystemTime
  draw
  t2 <- liftIO getSystemTime
  getEvtStrs >>= zipWithM_ (\i evtStr -> drawAt evtStr $ Coords i 0) [0..]
  (mayNewBufferSz, res) <- renderToScreen
  either
    (\err -> do
      let msg = "Renderer error :" ++ err
      drawAt msg $ Coords 0 0
      liftIO $ putStrLn msg)
    (\(dtDelta, dtCmds, dtFlush) -> gets appStateDebug >>= \case
        (Debug True) -> liftIO $ putStrLn $ " d "   ++ showTime' (t1...t2)
                                 ++ " de "  ++ showTime' dtDelta
                                 ++ " cmd " ++ showTime' dtCmds
                                 ++ " fl "  ++ showTime' dtFlush
        (Debug False) -> return ())
    res
  maybe
    (return ())
    (\_ -> asks writeToClient' >>= \f -> f $ FromClient CanvasSizeChanged)
      mayNewBufferSz
 where
  showTime' = justifyR 11 . showTime

{-# INLINABLE getEvtStrs #-}
getEvtStrs :: MonadState (AppState g) m
              => m [ColorString]
getEvtStrs =
  get >>= \(AppState _ _ _ _ h r _ _ _ _) ->
    return $ case r of
      Record -> multiLine 150 $ toColorStr h -- TODO screen width should be dynamic
      DontRecord -> []

{-# INLINABLE addEvent #-}
addEvent :: (GameLogic g)
         => UpdateEvent g -> AppState g -> AppState g
addEvent e s =
  s { eventHistory = addEventRepr (evtCategory e) $ eventHistory s }

addIgnoredOverdues :: MonadState (AppState g) m
                   => Int -> m ()
addIgnoredOverdues n =
  modify' $
    \s@(AppState _ _ _ _ hist _ _ _ _ _) ->
      s { eventHistory = iterate (addEventRepr IgnoredOverdue) hist !! n }

toColorStr :: OccurencesHist -> ColorString
toColorStr (OccurencesHist []    tailStr) = tailStr
toColorStr (OccurencesHist (x:_) tailStr) = tailStr <> toColorStr' x

addEventRepr :: EventCategory -> OccurencesHist -> OccurencesHist
addEventRepr e oh@(OccurencesHist h r) =
  case h of
    [] -> mkOccurencesHist (Occurences 1 e)
    Occurences n o:xs -> if o == e
                            then
                              OccurencesHist (Occurences (succ n) o:xs) r
                            else
                              let prevTailStr = toColorStr oh
                              in OccurencesHist (Occurences 1 e:h) prevTailStr

createState :: Screen
            -> Debug
            -> Maybe (ConnectIdT (ServerT g))
            -> ServerView (ValuesT (ServerT g))
            -> Time Point System
            -> Maybe (PollContextT g)
            -> AppState g
createState screen dbg a b t mpc =
  let g = Game (ClientState Ongoing Excluded) screen (GameState Nothing mkZeroAnimation) mempty [] mempty a b Nothing mkChat
  in AppState t g mpc mkEmptyGroup mkEmptyOccurencesHist DontRecord (ParticleSystemKey 0) dbg mkEmptyInstruments mempty

toColorStr' :: Occurences EventCategory -> ColorString
toColorStr' (Occurences n e) =
  let r@(ColorString l) = evtRepToCS $ evtCatToRep e
      col = case l of
        [] -> white
        ((_,LayeredColor _ fg):_) -> fg
  in r <> colored (pack $ replicate (pred n) '.' ) col

mkOccurencesHist :: Occurences EventCategory -> OccurencesHist
mkOccurencesHist o =
  OccurencesHist [o] mempty

-- | Lookups the instrument in 'Instruments', if it is found it is returned,
-- else, if the supplied key is 'Just', the instrument is inserted
--   and the server is notified of the new association.
useInstrument :: (GameLogicT e ~ g, s ~ ServerT g
                , MonadState (AppState g) m
                , MonadReader e m, Client e
                , MonadIO m)
              => Maybe MidiSourceIdx
              -- ^ The 'MidiSourceIdx' that will be used to copmute a new 'InstrumentId' if needed.
              -> Instrument
              -> m (Maybe InstrumentId)
useInstrument mayMidiIdx i = do
  is <- gets appInstruments
  -- note that if 2 clients want to use the same new instrument at the same time,
  -- we will end up having 2 different ids for it.
  -- It's not supposed to be a big problem, though.
  maybe
    (maybe
      (return Nothing)
      (\midiIdx -> do
          iid <- state $ \s ->
            let (newInstruments, key) = insertInstrument midiIdx i $ appInstruments s
            in (key, s { appInstruments = newInstruments })
          asks sendToServer' >>= \f -> f $ RegisterInstrument iid i
          return $ Just iid)
      mayMidiIdx)
    (return . Just)
    $ lookupInstrument i is
