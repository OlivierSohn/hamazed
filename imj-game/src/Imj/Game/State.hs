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
      , toggleRecordEvent
      , addIgnoredOverdues
      -- * utilities
      , mkAnim
      -- * reexports
      , module Imj.Game.Types
      ) where

import           Imj.Prelude
import           Prelude(putStr, putStrLn, length)

import           Control.Monad.State.Class(MonadState)
import           Control.Monad.State.Strict(state, get, put)
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(asks)
import           Data.Text(pack)

import           Imj.Categorized
import           Imj.Control.Concurrent.AsyncGroups.Class
import           Imj.Geo.Discrete.Types
import           Imj.Event
import           Imj.Game.Configuration
import           Imj.Game.Types
import           Imj.Graphics.Screen
import           Imj.Input.Types
import           Imj.Server.Class
import           Imj.Server.Types
import           Imj.ServerView.Types

import           Imj.Game.Audio.Class
import           Imj.Game.Command
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

{-# INLINABLE onEvent #-}
onEvent :: (GameLogicT e ~ g
          , MonadState (AppState g) m
          , MonadReader e m, Client e, Render e, PlayerInput e, HasSizedFace e, AsyncGroups e, Audio e
          , MonadIO m)
        => Maybe (GenEvent g)
        -> m ()
onEvent mayEvt = do
  checkPlayerEndsProgram
  debug >>= \case
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
onEvent' :: (GameLogicT e ~ g
           , MonadState (AppState g) m
           , MonadReader e m, Client e, Render e, HasSizedFace e, AsyncGroups e, Audio e
           , MonadIO m)
         => Maybe (GenEvent g) -> m ()
onEvent' = maybe (handleEvent Nothing) -- if a rendergroup exists, render and reset the group
  (\case
    CliEvt e -> asks sendToServer' >>= \f -> f e
    Evt ToggleEventRecording -> state toggleRecordEvent
    Evt    evt -> onUpdate $ Right evt
    SrvEvt evt -> onUpdate $ Left evt)
 where
  onUpdate e = do
    getRecording >>= \case
      Record -> state $ addEvent e
      DontRecord -> return ()
    handleEvent $ Just e

{-# INLINABLE handleEvent #-}
handleEvent :: (GameLogicT e ~ g
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
  get >>= \(AppState t a (EventGroup d e prevT f) b c g de) ->
    put $ AppState t a (EventGroup d e (add |+| prevT) f) b c g de

{-# INLINABLE addToCurrentGroupOrRenderAndStartNewGroup #-}
addToCurrentGroupOrRenderAndStartNewGroup :: (GameLogicT e ~ g
                                            , MonadState (AppState g) m
                                            , MonadReader e m, Render e, Client e
                                            , MonadIO m)
                                          => Maybe (UpdateEvent g) -> m ()
addToCurrentGroupOrRenderAndStartNewGroup evt =
  get >>= \(AppState prevTime _ prevGroup _ _ _ _) -> do
    let onRender = do
          debug >>= \case
            (Debug True) -> liftIO $ putStr $ groupStats prevGroup
            (Debug False) -> return ()
          renderAll
          liftIO (tryGrow evt mkEmptyGroup) >>= maybe
            (error "growing an empty group never fails")
            return
    liftIO (tryGrow evt prevGroup) >>= maybe
      (onRender >>= \group -> liftIO getSystemTime >>= \curTime -> return (curTime, group))
      (return . (,) prevTime)
    >>= \(t,g) -> get >>= \(AppState _ a _ b c d e) -> put $ AppState t a g b c d e


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
    (\(dtDelta, dtCmds, dtFlush) -> debug >>= \case
        (Debug True) -> liftIO $ putStrLn $ " d "   ++ showTime' (t1...t2)
                                 ++ " de "  ++ showTime' dtDelta
                                 ++ " cmd " ++ showTime' dtCmds
                                 ++ " fl "  ++ showTime' dtFlush
        (Debug False) -> return ())
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
  get >>= \(AppState _ _ _ h r _ _) ->
    return $ case r of
      Record -> multiLine 150 $ toColorStr h -- TODO screen width should be dynamic
      DontRecord -> []

{-# INLINABLE getRecording #-}
getRecording :: MonadState (AppState g) m
             => m RecordMode
getRecording = do
  (AppState _ _ _ _ record _ _) <- get
  return record

{-# INLINABLE addEvent #-}
addEvent :: (GameLogic g)
         => UpdateEvent g -> AppState g -> ((), AppState g)
addEvent e (AppState t g evts es r b d) =
  let es' = addEventRepr (evtCategory e) es
  in ((), AppState t g evts es' r b d)

toggleRecordEvent :: AppState g -> ((), AppState g)
toggleRecordEvent (AppState t g e _ r b d) =
  let r' = case r of
        Record -> DontRecord
        DontRecord -> Record
  in ((), AppState t g e mkEmptyOccurencesHist r' b d)

addIgnoredOverdues :: MonadState (AppState g) m
                   => Int -> m ()
addIgnoredOverdues n =
  get >>= \(AppState t a e hist record b d) -> do
    let hist' = iterate (addEventRepr IgnoredOverdue) hist !! n
    put $ AppState t a e hist' record b d

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
            -> ServerView (ServerT g)
            -> ConnectionStatus
            -> IO (AppState g)
createState screen dbg a b c = do
  let g  = Game (ClientState Ongoing Excluded) screen (GameState Nothing mkZeroAnimation) mempty [] mempty a b c mkChat
  t <- getSystemTime
  return $ AppState t g mkEmptyGroup mkEmptyOccurencesHist DontRecord (ParticleSystemKey 0) dbg

{-# INLINABLE debug #-}
debug :: MonadState (AppState g) m => m Debug
debug =
  get >>= \(AppState _ _ _ _ _ _ d) -> return d

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

mkEmptyOccurencesHist :: OccurencesHist
mkEmptyOccurencesHist = OccurencesHist [] mempty