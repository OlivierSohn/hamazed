{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Imj.Game.Loop
      ( loop
      ) where

import           Imj.Prelude

import           Control.Concurrent(forkIO, threadDelay)
import           Control.Concurrent.Async(wait, withAsync, async, cancel) -- I can't use UnliftIO because I have State here
import           Control.Concurrent.STM(STM, check, atomically, readTQueue, readTVar, registerDelay)
import           Control.Monad.Reader.Class(MonadReader, asks)
import           Control.Monad.State.Class(gets)
import           Data.IORef(newIORef, atomicModifyIORef', atomicWriteIORef)

import           Imj.Input.Types

import           Imj.Event
import           Imj.Game.Deadlines
import           Imj.Game.State

loop :: (MonadIO m
       , g ~ GameLogicT e
       , MonadState (AppState g) m
       , MonadReader e m, PlayerInput e, Client e)
     => (PlatformEvent -> m [GenEvent g])
       -- ^ Translates a 'PlatformEvent' to a 'GenEvent'
     -> (Maybe (GenEvent g) -> m ())
       -- ^ Handles a 'GenEvent'
     -> m ()
loop liftPlatformEvent onEventF =
  forever $ nextEvent >>= mapM_ onEventF
 where
  nextEvent = produceEvent >>= maybe
    (return [Nothing]) -- means we need to render now.
    (either
      (\k -> liftPlatformEvent k >>= \case
        [] -> nextEvent -- the event was unknown, retry.
        l -> return $ map Just l)
      (return . (:[]) . Just))


type AnyEvent g = Either PlatformEvent (GenEvent g)

-- | MonadState (AppState s) is needed to know if the level is finished or not.
{-# INLINABLE produceEvent #-}
produceEvent :: (MonadIO m
               , g ~ GameLogicT e
               , MonadState (AppState g) m
               , MonadReader e m, PlayerInput e, Client e)
             => m (Maybe (AnyEvent g))
produceEvent = do
  server <- asks serverQueue
  platform <- asks plaformQueue

  asks queueType >>= \case
    AutomaticFeed -> return ()
    ManualFeed -> asks pollKeys >>= liftIO

  let dispatch (FromServer e) = SrvEvt e
      dispatch (FromClient e) = Evt e
      readInput = fmap (Right . dispatch) (readTQueue server) -- this queue is fed by the server
              <|> fmap Left (readTQueue platform) -- this queue is fed by the platform, and may need polling / waiting for events to be fed

  -- We handle pending input events first: they have a higher priority than any other.
  liftIO (tryAtomically readInput) >>= maybe
    (liftIO getSystemTime >>= getNextDeadline >>= maybe
      (triggerRenderOr $ Just <$> atomically readInput)
      (\case
        Overdue d ->
          return $ Just $ Right $ Evt $ Timeout d
        Future (Deadline deadlineTime _ _) ->
          triggerRenderOr $ tryAtomicallyBefore deadlineTime readInput))
    (return . Just)

triggerRenderOr :: (MonadIO m
                  , MonadState (AppState g) m
                  , MonadReader e m, PlayerInput e)
                => IO (Maybe (AnyEvent g))
                -> m (Maybe (AnyEvent g))
triggerRenderOr readInput = visible <$> gets eventsGroup >>= \needsRender ->
  if needsRender
    then -- we can't afford to wait, we force a render
      return Nothing
    else
      asks queueType >>= \case
        AutomaticFeed ->
          liftIO readInput
        ManualFeed -> do
          stopWaiting <- asks stopWaitKeys
          waitEvts <- asks waitKeys
          countStopped <- liftIO $ newIORef (0 :: Int)
          liftIO $ withAsync
            (do
              -- forked thread
              res <- readInput
              asy <- liftIO $ do
                atomicWriteIORef countStopped 1 -- it is important to do this /before/ 'stopWaiting'
                stopWaiting
                -- workaround for <https://github.com/glfw/glfw/issues/1281 this glfw bug>:
                -- due to a race condition between stopWaiting and waitEvts, we need to call stopWaiting one more time.
                -- Note that we could disable this workaround when not on linux/X11.
                async $ threadDelay 1000 >> stopWaiting
              return (res,asy))
            (\a -> do
              -- main thread
              let go = do
                    waitEvts
                    liftIO (atomicModifyIORef' countStopped (\v -> (v,v))) >>= \case
                      0 -> go
                      _ -> liftIO (wait a >>= \(res,asy) -> do
                        void $ forkIO $ cancel asy
                        return res)
              go)

{-# INLINABLE tryAtomically #-}
tryAtomically :: STM (AnyEvent g)
              -> IO (Maybe (AnyEvent g))
tryAtomically a =
  atomically $ fmap Just a
           <|> return Nothing

{-# INLINABLE tryAtomicallyBefore #-}
tryAtomicallyBefore :: Time Point System
                    -> STM (AnyEvent g)
                    -> IO (Maybe (AnyEvent g))
tryAtomicallyBefore t a =
  getDurationFromNowTo t >>= \allowed ->
    if strictlyNegative allowed
      then
        return Nothing
      else
        registerDelay (fromIntegral $ toMicros allowed) >>= \timeout ->
          atomically $ fmap Just a
                   <|> (return Nothing << check =<< readTVar timeout)
infixr 1 <<
{-# INLINE (<<) #-}
(<<) :: (Monad m) => m b -> m a -> m b
b << a = a >> b
