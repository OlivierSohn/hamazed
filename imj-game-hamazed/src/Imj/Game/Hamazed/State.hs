{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

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
import           Control.Monad.State.Strict(state, get, put)
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(asks)
import           Data.Text(pack)

import           Imj.Categorized
import           Imj.Client.Class
import           Imj.Client.Types
import           Imj.Game.Hamazed.State.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Game.Hamazed.Types
import           Imj.Input.Types
import           Imj.ServerView.Types

import           Imj.Game.Hamazed.Loop.Create
import           Imj.Game.Hamazed.Loop.Draw
import           Imj.Game.Hamazed.Loop.Event
import           Imj.Game.Hamazed.Loop.Update
import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.Class.HasSizedFace
import           Imj.Graphics.Class.Words hiding (length)
import           Imj.Graphics.Color
import           Imj.Graphics.Render.FromMonadReader
import           Imj.Graphics.Text.ColorString hiding(putStrLn, putStr)
import           Imj.Graphics.Text.Render
import           Imj.Input.FromMonadReader

{-# INLINABLE onEvent #-}
onEvent :: (ServerT e ~ Hamazed
          , CliEvtT e ~ Event evt
          , Categorized evt
          , MonadState (AppState evt) m
          , MonadReader e m, Client e, Render e, PlayerInput e, HasSizedFace e
          , MonadIO m)
        => (evt -> m ())
        -> (ServerEventT (ServerT e) -> m ())
        -> Maybe (GenEvent e)
        -> m ()
onEvent f f' mayEvt = do
  checkPlayerEndsProgram
  debug >>= \case
    True -> liftIO $ putStrLn $ show mayEvt -- TODO make this more configurable (use levels 1, 2 of debugging)
    False -> return ()
  onEvent' f f' mayEvt
 where
  checkPlayerEndsProgram =
    playerEndsProgram >>= \end ->
      when end $ sendToServer $ RequestApproval $ Leaves $ Right () -- Note that it is safe to send this several times

{-# INLINABLE onEvent' #-}
onEvent' :: (ServerT e ~ Hamazed
            , CliEvtT e ~ Event evt
            , Categorized evt
            , MonadState (AppState evt) m
            , MonadReader e m, Client e, Render e, HasSizedFace e
            , MonadIO m)
         => (evt -> m ())
         -> (ServerEventT (ServerT e) -> m ())
         -> Maybe (GenEvent e) -> m ()
onEvent' f f' Nothing = handleEvent f f' Nothing -- if a rendergroup exists, render and reset the group
onEvent' _ _ (Just (CliEvt clientEvt)) = sendToServer clientEvt
onEvent' _ _ (Just (Evt ToggleEventRecording)) = state toggleRecordEvent
onEvent' f f' (Just (Evt    evt)) = onUpdateEvent f f' $ Right evt
onEvent' f f' (Just (SrvEvt evt)) = onUpdateEvent f f' $ Left evt

{-# INLINABLE onUpdateEvent #-}
onUpdateEvent :: (ServerT e ~ Hamazed
                , CliEvtT e ~ Event evt
                , Categorized evt
                , MonadState (AppState evt) m
                , MonadReader e m, Client e, Render e, HasSizedFace e
                , MonadIO m)
              => (evt -> m ())
              -> (ServerEventT (ServerT e) -> m ())
              -> UpdateEvent Hamazed (CliEvtT e) -> m ()
onUpdateEvent f f' e = do
  getRecording >>= \case
    Record -> state $ addEvent e
    DontRecord -> return ()
  handleEvent f f' $ Just e

{-# INLINABLE handleEvent #-}
handleEvent :: (ServerT e ~ Hamazed
              , CliEvtT e ~ Event evt
              , MonadState (AppState evt) m
              , MonadReader e m, Client e, Render e, HasSizedFace e
              , MonadIO m)
            => (evt -> m ())
            -> (ServerEventT (ServerT e) -> m ())
            -> Maybe (UpdateEvent Hamazed (CliEvtT e)) -> m ()
handleEvent f f' e = do
  addToCurrentGroupOrRenderAndStartNewGroup e
  maybe
    (return ())
    (\evt -> do
      t1 <- liftIO getSystemTime
      updateAppState evt f f'
      t2 <- liftIO getSystemTime
      addUpdateTime $!! t1...t2)
    e

{-# INLINE addUpdateTime #-}
addUpdateTime :: MonadState (AppState evt) m
              => Time Duration System -> m ()
addUpdateTime add =
  get >>= \(AppState t a (EventGroup d e prevT f) b c g de) ->
    put $ AppState t a (EventGroup d e (add |+| prevT) f) b c g de

{-# INLINABLE addToCurrentGroupOrRenderAndStartNewGroup #-}
addToCurrentGroupOrRenderAndStartNewGroup :: (CliEvtT e ~ Event evt
                                            , MonadState (AppState evt) m
                                            , MonadReader e m, Render e, Client e
                                            , MonadIO m)
                                          => Maybe (UpdateEvent Hamazed (Event evt)) -> m ()
addToCurrentGroupOrRenderAndStartNewGroup evt =
  get >>= \(AppState prevTime _ prevGroup _ _ _ _) -> do
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
    >>= \(t,g) -> get >>= \(AppState _ a _ b c d e) -> put $ AppState t a g b c d e


groupStats :: EventGroup s c -> String
groupStats (EventGroup l _ t _) =
  replicate (pred $ length l) ' ' ++
  "|" ++
  replicate (10 - length l) ' ' ++
  " u " ++
  showTime t

{-# INLINABLE renderAll #-}
renderAll :: (CliEvtT e ~ Event evt -- TODO remove by passing Hamazed as parameter in Env and AppState
            , MonadState (AppState evt) m
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
        True -> liftIO $ putStrLn $ " d "   ++ showTime' (t1...t2)
                                 ++ " de "  ++ showTime' dtDelta
                                 ++ " cmd " ++ showTime' dtCmds
                                 ++ " fl "  ++ showTime' dtFlush
        False -> return ())
    res
  maybe
    (return ())
    (\_ -> asks writeToClient' >>= \f -> f $ FromClient CanvasSizeChanged)
      mayNewBufferSz
 where
  showTime' = justifyR 11 . showTime

{-# INLINABLE getEvtStrs #-}
getEvtStrs :: MonadState (AppState evt) m
              => m [ColorString]
getEvtStrs =
  get >>= \(AppState _ _ _ h r _ _) ->
    return $ case r of
      Record -> multiLine 150 $ toColorStr h -- TODO screen width should be dynamic
      DontRecord -> []

{-# INLINABLE getRecording #-}
getRecording :: MonadState (AppState evt) m
             => m RecordMode
getRecording = do
  (AppState _ _ _ _ record _ _) <- get
  return record

addEvent :: Categorized evt
         => UpdateEvent Hamazed (Event evt) -> (AppState evt) -> ((), (AppState evt))
addEvent e (AppState t g evts es r b d) =
  let es' = addEventRepr (evtCategory e) es
  in ((), AppState t g evts es' r b d)

toggleRecordEvent :: (AppState evt) -> ((), (AppState evt))
toggleRecordEvent (AppState t g e _ r b d) =
  let r' = case r of
        Record -> DontRecord
        DontRecord -> Record
  in ((), AppState t g e mkEmptyOccurencesHist r' b d)

addIgnoredOverdues :: MonadState (AppState evt) m
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

createState :: Maybe Size
            -> Bool
            -> SuggestedPlayerName
            -> ServerView ColorScheme WorldParameters
            -> ConnectionStatus
            -> IO (AppState evt)
createState ms dbg a b c = do
  g <- initialGame ms a b c
  t <- getSystemTime
  return $ AppState t g mkEmptyGroup mkEmptyOccurencesHist DontRecord (ParticleSystemKey 0) dbg

{-# INLINABLE debug #-}
debug :: MonadState (AppState evt) m => m Bool
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
