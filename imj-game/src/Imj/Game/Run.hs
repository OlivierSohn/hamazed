{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Imj.Game.Run
      ( runWith
      , produceEvent
      ) where

import           Imj.Prelude

import           Control.Concurrent(threadDelay)
import           Control.Concurrent.Async(withAsync, wait, race) -- I can't use UnliftIO because I have State here
import           Control.Concurrent.STM(STM, check, atomically, readTQueue, readTVar, registerDelay)
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader, asks)
import           Control.Monad.Reader(runReaderT)
import           Control.Monad.State.Class(MonadState)
import           Control.Monad.State.Strict(runStateT)

import           Imj.Server.Class
import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Screen
import           Imj.Input.Types
import           Imj.ServerView.Types

import           Imj.Event
import           Imj.Game.Env
import           Imj.Game.KeysMaps
import           Imj.Game.Deadlines
import           Imj.Game.State
import           Imj.Graphics.Class.HasSizedFace
import           Imj.Graphics.Font
import           Imj.Graphics.Render.Delta
import           Imj.Graphics.Text.RasterizedString
import           Imj.Game.Network.ClientQueues

{-# INLINABLE runWith #-}
runWith :: (GameLogic g
          , PlayerInput i, DeltaRenderBackend i)
        => Bool
        -> ClientQueues g
        -> ServerView (ServerT g)
        -> ConnectIdT (ServerT g)
        -> i
        -> IO ()
runWith debug queues srv player backend =
  withTempFontFile font fontname $ \path -> withFreeType $ withSizedFace path (Size 16 16) $ \face ->
    flip withDefaultPolicies backend $ \drawEnv -> do
      screen <- mkScreen <$> getDiscreteSize backend
      env <- mkEnv drawEnv backend queues face
      void $ createState screen debug player srv NotConnected >>=
        runStateT (runReaderT (loop translatePlatformEvent onEvent) env)

 where

  (font,fontname) = fromMaybe (error "absent font") $ look "LCD" fontFiles

  look _ [] = Nothing
  look name ((f,ftName,_):rest) =
    if ftName == name
      then Just (f,ftName)
      else look name rest

loop :: (MonadIO m
       , g ~ GameLogicT e
       , MonadState (AppState g) m
       , MonadReader e m, PlayerInput e, Client e)
     => (PlatformEvent -> m (Maybe (GenEvent g)))
       -- ^ Translates a 'PlatformEvent' to a 'GenEvent'
     -> (Maybe (GenEvent g) -> m ())
       -- ^ Handles a 'GenEvent'
     -> m ()
loop liftPlatformEvent onEventF =
  forever $ nextEvent >>= onEventF
 where
  nextEvent = produceEvent >>= maybe
    (return Nothing) -- means we need to render now.
    (either
      (\k -> liftPlatformEvent k >>= maybe
        nextEvent -- the event was unknown, retry.
        (return . Just))
      (return . Just))


type AnyEvent g = Either PlatformEvent (GenEvent g)

-- stats of CPU usage in release, when using 'race (wait res) (threadDelay x)':
-- 10000 ->   3.5% -- ok but 10 ms is a lot
--  1000 ->  18.0%
--   100 ->  20.7%
--     1 -> 117.0%

-- stats of CPU usage in release, when using above with 1 and additionnal threadDelay x)':
-- 10000 ->   4.7%
--  1000 ->  23.7%
--   100 ->  23.7%
--     1 -> 118.0%


-- using 'race (wait res) (threadDelay x)' incurs an overhead: if we don't use it,
-- with glfw:
-- using poll + threadDelay 10000 ->   2.7%
-- using poll + threadDelay  1000 ->  17.2%
-- using poll + threadDelay   100 ->  16.8%
-- using poll + threadDelay    10 ->  82.0%
-- using poll + threadDelay    1  -> 111.0%

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
      readInput = fmap (Right . dispatch) (readTQueue server)
              <|> fmap Left (readTQueue platform)

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
triggerRenderOr readInput = hasVisibleNonRenderedUpdates >>= \needsRender ->
  if needsRender
    then -- we can't afford to wait, we force a render
      return Nothing
    else
      asks queueType >>= getWaitForResult >>= liftIO . withAsync readInput
 where
  getWaitForResult = \case
    AutomaticFeed -> return wait -- 0% CPU usage while waiting
    ManualFeed -> do
      --waitKT <- asks waitKeysTimeout
      polling <- asks pollKeys
      return $ waitWithPolling polling
     where
      waitWithPolling polling a = go
       where
        go =
      -- Using 100 microseconds as minimum interval between consecutive 'pollPlayerEvents'
      -- seems to be a good trade-off between "CPU usage while waiting" and reactivity.
      -- There are 3 alternatives hereunder, each of them has a different CPU cost.
      -- I chose the one that is both reasonnably economical and allows to
      -- save up-to 100 micro seconds latency. I left the other alternatives commented out
      -- with measured CPU usage for reference.
        --{-
        -- [alternative 1] 20.3% CPU while waiting
          race (wait a) (threadDelay 100) >>= either
            return
            (\_ -> polling >> go)
        --}
        {-
          poll res >>= maybe
            (do --waitKT (fromSecs 0.0001) -- [alternative 2] 55% CPU while waiting
                threadDelay 100 >> pollK -- [alternative 3] 15 % CPU while waiting
                go)
            (\case
                Left e -> throwIO e
                Right r -> return r)
        -}

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
