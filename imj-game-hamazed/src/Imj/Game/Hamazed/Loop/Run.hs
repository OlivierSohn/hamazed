{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Imj.Game.Hamazed.Loop.Run
      ( run
      , produceEvent
      ) where

import           Imj.Prelude
import           Prelude (putStrLn, getLine, toInteger)

import           Control.Concurrent(threadDelay)
import           Control.Concurrent.Async(withAsync, wait, race)
import           Control.Concurrent.STM(check, atomically, readTQueue, readTVar, registerDelay)
import           Control.Monad(join)
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader, asks)
import           Control.Monad.Reader(runReaderT)
import           Control.Monad.State.Class(MonadState)
import           Control.Monad.State.Strict(runStateT)
import           Data.Char(toLower)
import           Options.Applicative
                  (progDesc, fullDesc, info, header, customExecParser, prefs, helper
                  , showHelpOnError, short, long, option, str, help, optional
                  , ReadM, readerError, (<*>), switch)
import           System.Info(os)
import           System.IO(hFlush, stdout)
import           Text.Read(readMaybe)

import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Geo.Discrete.Types
import           Imj.Input.Types

import           Imj.Game.Hamazed.Env
import           Imj.Game.Hamazed.KeysMaps
import           Imj.Game.Hamazed.Loop.Deadlines
import           Imj.Game.Hamazed.Loop.Event
import           Imj.Game.Hamazed.Network.Class.ClientNode
import           Imj.Game.Hamazed.Network.GameNode
import           Imj.Game.Hamazed.Network.Server
import           Imj.Game.Hamazed.State
import           Imj.Graphics.Render
import           Imj.Graphics.Render.Delta

{- | Runs the Hamazed game.

If you chose to run in the terminal, and your terminal window is too small, the
program will error and tell you what is the minimum window size to run the game.

The game
<https://ghc.haskell.org/trac/ghc/ticket/7353 doesn't run on Windows>.
-}
run :: IO ()
run =
  if os == "mingw32"
    then
      putStrLn $ "Windows is not currently supported"
      ++ " (https://ghc.haskell.org/trac/ghc/ticket/7353)."
    else
      runWithArgs

data BackendType =
    Console
  | OpenGLWindow
  deriving (Show)

runWithArgs :: IO ()
runWithArgs =
  join . customExecParser (prefs showHelpOnError) $
    info (helper <*> parser)
    (  fullDesc
    <> header "imj-game-hamazed-exe runs the 'Hamazed' game."
    <> progDesc "Hamazed is a game with flying numbers and 8-bit color animations."
    )
 where
  parser =
    runWithBackend
      <$> optional
             (option backendArg (long "render"
                              <> short 'r'
                              <> help (
                              "Use 'console' to play in the console, " ++
                              "use 'opengl' to play in an opengl window. " ++
                              renderHelp
                              )))
      <*> optional
             (option srvNameArg (long "gameHostName"
                              <> short 'n'
                              <> help (
                              "Omit this option to run a game server on your machine, and connect to it. " ++
                              "Use \"localhost\" (or 127.0.0.1) to connect to a server already running on your machine. " ++
                              "Use the IP or DNS name of a distant server to connect to it."
                              )))
      <*> optional
             (option srvPortArg (long "gameHostPort"
                              <> short 'p'
                              <> help (
                              "The port number of the listening socket of the game server. " ++
                              "Default is " ++ show (toInteger defaultPort) ++ ". "
                              )))
      <*> optional
             (option suggestedPlayerName (long "player"
                              <> help (
                              "The name of the player you want to use, " ++
                              "in a multiplayer context. Default is \"player\"."
                              )))
      <*> switch ( long "debug" <> short 'd' <> help "Print debug infos in the terminal." )

renderHelp :: String
renderHelp =
  "\nAccepted synonyms of 'console' are 'ascii', 'term', 'terminal'." ++
  "\nAccepted synonyms of 'opengl' are 'win', 'window'."

backendArg :: ReadM BackendType
backendArg =
  str >>= \s -> case map toLower s of
    "ascii"        -> return Console
    "console"      -> return Console
    "term"         -> return Console
    "terminal"     -> return Console
    "opengl"       -> return OpenGLWindow
    "win"          -> return OpenGLWindow
    "window"       -> return OpenGLWindow
    st -> readerError $ "Encountered an invalid render type:\n\t"
                    ++ show st
                    ++ "\nAccepted render types are 'console' and 'opengl'."
                    ++ renderHelp

srvNameArg :: ReadM ServerName
srvNameArg =
  str >>= \s -> case map toLower s of
    [] -> readerError $ "Encountered an empty servername. Accepted names are ip address or domain name."
                     ++ renderHelp
    name -> return $ ServerName name

srvPortArg :: ReadM ServerPort
srvPortArg =
  str >>= \s -> case map toLower s of
    [] -> readerError $ "Encountered an empty serverport."
                     ++ renderHelp
    name ->
      maybe
        (error $ "invalid number : " ++ show name)
        (return . ServerPort)
          (readMaybe name)

suggestedPlayerName :: ReadM SuggestedPlayerName
suggestedPlayerName =
  str >>= \case
    [] -> readerError $ "Encountered an empty player name."
                     ++ renderHelp
    name -> return $ SuggestedPlayerName name

userPicksBackend :: IO BackendType
userPicksBackend = do
  putStrLn ""
  putStrLn " Welcome to Hamazed!"
  putStrLn ""
  putStrLn " - Press (1) then (Enter) to play in the console."
  putStrLn "     An error message will inform you if your console is too small."
  putStrLn "          [Equivalent to passing '-r console']"
  putStrLn " - Press (2) then (Enter) to play in a separate window (enables more rendering options)."
  putStrLn "          [Equivalent to passing '-r opengl']"
  putStrLn ""
  hFlush stdout -- just in case stdout BufferMode is "block"
  getLine >>= \case
    "1" -> return Console
    "2" -> return OpenGLWindow
    c -> putStrLn ("invalid value : " ++ c) >> userPicksBackend

runWithBackend :: Maybe BackendType
               -> Maybe ServerName
               -> Maybe ServerPort
               -> Maybe SuggestedPlayerName
               -> Bool
               -> IO ()
runWithBackend maybeBackend maySrvName maySrvPort mayPlayerName debug = do
  putStrLn $ " ------------- --------------------------"
  putStrLn $ "| Backend     : " ++ show maybeBackend
  putStrLn $ "| Server name : " ++ show maySrvName
  putStrLn $ "| Server port : " ++ show maySrvPort
  putStrLn $ "| Debug       : " ++ show debug
  putStrLn $ " ------------- --------------------------"

  let srvPort = fromMaybe defaultPort maySrvPort
      srv = mkServer maySrvName srvPort
      spn = fromMaybe "player" $ mayPlayerName
  maybe userPicksBackend return maybeBackend >>= \case
    Console      -> newConsoleBackend
      >>= runWith debug srv spn
    OpenGLWindow -> newOpenGLBackend "Hamazed" 10 (Size 600 1400)
      >>= runWith debug srv spn


{-# INLINABLE runWith #-}
runWith :: (PlayerInput a, DeltaRenderBackend a)
        => Bool
        -> Server
        -> SuggestedPlayerName
        -> a
        -> IO ()
runWith debug srv player backend =
  flip withDefaultPolicies backend $ \drawEnv -> do
    env <- Env drawEnv backend <$> startNetworking player srv
    sz <- getDiscreteSize backend
    _ <- createState sz debug player srv NotConnected >>=
      runStateT (runReaderT loop env)
    return ()

loop :: (MonadState AppState m, MonadIO m, MonadReader e m, ClientNode e, Render e, PlayerInput e)
     => m ()
loop = do
  let prod =
        produceEvent >>= maybe
          (return Nothing) -- means we need to render now.
          (either
            (\k -> eventFromKey k >>= maybe
              prod -- the key was unknown, retry.
              (return . Just))
            (return . Just))
  prod >>= \case
    (Just (SrvEvt DisconnectionAccepted)) -> return ()
    x -> do
      onEvent x
      loop


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

-- | MonadState AppState is needed to know if the level is finished or not.
{-# INLINABLE produceEvent #-}
produceEvent :: (MonadState AppState m, MonadReader e m, PlayerInput e, ClientNode e, MonadIO m)
             => m (Maybe (Either Key GenEvent))
produceEvent = do
  server <- asks serverQueue
  keys <- asks keysQueue
  -- TODO try factorizing:
--  let stmAct = fmap (Right . SrvEvt)  (readTQueue a)
--           <|> fmap (Left) (readTQueue b)
  qt <- asks queueType
  pollK <- asks pollKeys
  case qt of
    AutomaticFeed -> return ()
    PollOrWaitOnEvents -> liftIO pollK

  -- We handle pending input events first: they have a higher priority than any other.
  liftIO (tryGetInputEvent server keys) >>= \case
    Just x -> return $ Just x
    Nothing -> do
      let whenWaitingIsAllowed mayTimeLimit = hasVisibleNonRenderedUpdates >>= \needsRender ->
            if needsRender
              then
                -- we can't afford waiting, we force a render
                return Nothing
              else do
                --waitKT <- asks waitKeysTimeout

                liftIO $ do
                  let x = maybe
                        (Just <$> getInputEvent server keys)
                        (\t -> getInputEventBefore t server keys)
                          mayTimeLimit

                  withAsync x $ \res -> do
                    let go = do
                          case qt of
                            AutomaticFeed -> wait res -- 0% CPU usage while waiting
                            PollOrWaitOnEvents ->
                          -- Using 100 microseconds as minimum interval between consecutive 'pollPlayerEvents'
                          -- seems to be a good trade-off between CPU usage while waiting
                          -- and reactivity.
                          -- There are 3 alternatives hereunder, each of them has a different CPU cost.
                          -- I chose the one that is both reasonnably economical and allows to
                          -- save up-to 100 micro seconds latency. I left the other alternatives commented out
                          -- with measured CPU usage for reference.
                            --{-
                            -- [alternative 1] 20.3% CPU while waiting
                              race (wait res) (threadDelay 100) >>= either return (\_ -> pollK >> go)
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
                    go

      liftIO getSystemTime >>= getNextDeadline >>= maybe
        (whenWaitingIsAllowed Nothing)
        (\case
          Overdue d ->
            return $ Just $ Right $ Evt $ Timeout d
          Future (Deadline deadlineTime _ _) ->
            whenWaitingIsAllowed (Just deadlineTime))

-- | First tries to get pending 'ServerEvent' then tries to get pending player input
tryGetInputEvent :: TQueue ServerEvent
                 -> TQueue Key
                 -> IO (Maybe (Either Key GenEvent))
tryGetInputEvent a b = do
  atomically $ fmap (Just . Right . SrvEvt)  (readTQueue a)
           <|> fmap (Just . Left) (readTQueue b)
           <|> return Nothing

getInputEvent :: TQueue ServerEvent
              -> TQueue Key
              -> IO (Either Key GenEvent)
getInputEvent a b = do
  atomically $ fmap (Right . SrvEvt) (readTQueue a)
           <|> fmap Left (readTQueue b)

getInputEventBefore :: Time Point System
                    -> TQueue ServerEvent
                    -> TQueue Key
                    -> IO (Maybe (Either Key GenEvent))
getInputEventBefore t a b =
  getDurationFromNowTo t >>= \allowed ->
    if strictlyNegative allowed
      then
        return Nothing
      else do
        registerDelay (fromIntegral $ toMicros allowed) >>= \timeout ->
          atomically $ fmap (Just . Right . SrvEvt) (readTQueue a)
                   <|> fmap (Just . Left)  (readTQueue b)
                   <|> (return Nothing << check =<< readTVar timeout)
infixr 1 <<
{-# INLINE (<<) #-}
(<<) :: (Monad m) => m b -> m a -> m b
b << a = a >> b
