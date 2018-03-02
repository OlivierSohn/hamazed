{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Game.Hamazed.Loop.Run
      ( run
      , produceEvent
      ) where

import           Imj.Prelude
import           Prelude (putStrLn, getLine, toInteger)

import           Control.Concurrent(threadDelay, forkIO, readMVar, newEmptyMVar)
import           Control.Concurrent.Async(withAsync, wait, race)
import           Control.Concurrent.STM(check, atomically, readTQueue, readTVar, registerDelay)
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader, asks)
import           Control.Monad.Reader(runReaderT)
import           Control.Monad.State.Class(MonadState)
import           Control.Monad.State.Strict(runStateT)
import           Data.Char(toLower)
import           Data.Maybe(isJust)
import           Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map(fromList, lookup, keys)
import           Network.Socket(withSocketsDo)
import           Options.Applicative
                  (ParserHelp(..), progDesc, fullDesc, info, header, execParserPure, prefs, helper
                  , showHelpOnError, short, long, option, str, help, optional
                  , ReadM, readerError, (<*>), switch)
import           Options.Applicative.Extra(handleParseResult, overFailure)
import qualified Options.Applicative.Help as Appli (red)
import           System.Environment(getArgs)
import           System.Info(os)
import           System.IO(hFlush, stdout)
import           Text.Read(readMaybe)

import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Color.Types
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
run = withSocketsDo $
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
  join $ execParserPure (prefs showHelpOnError) parserInfo <$> getArgs >>=
    handleParseResult . overFailure (\ph -> ph {helpError = fmap Appli.red $ helpError ph})
 where
  parserInfo =
    info (helper <*> parser)
    (  fullDesc
    <> header (
       "**** imj-game-hamazed-exe runs the 'Hamazed' multiplayer game. " ++
       "Each client (player) is connected to a server."
      )
    <> progDesc (
       "If you want to: " ++
       "(1) Create just a server, use [--serverOnly] and optionally [--serverPort]. " ++
       "(2) Create just a client connected to an existing server, use [--serverName] and optionally [--serverPort]. " ++
       "(3) Create both a server and a client connected to it, use optionally [--serverPort]."
       ))
  parser =
    runWithBackend
      <$> switch
          (  long "serverOnly"
          <> short 's'
          <> help
          "Create - only - the server (no client). Incompatible with --serverName."
          )
      <*> optional
            (option srvNameArg
               (  long "serverName"
               <> short 'n'
               <> help (
               "Connect to a server " ++
               "(use \"localhost\" to target your machine). Incompatible with --serverOnly."
               )))
      <*> optional
            (option srvPortArg
               (  long "serverPort"
               <> short 'p'
               <> help (
               "Listening port number of the server to connect to, or to create. " ++
               "Default is " ++ show (toInteger defaultPort) ++ ".")
               ))
      <*> optional
            (option srvLogsArg
               (  long "serverLogs"
               <> short 'l'
               <> help (
               "'none': no server logs. 'console': server logs in the console. " ++ -- TODO merge with -d
               "Default is 'none'. Incompatible with --serverName."
               )))
      <*> optional
            (option srvColorSchemeArg
               (  long "colorScheme"
               <> short 'c'
               <> help (
               "Defines the colors of ships. Possible values are: " ++ descPredefinedColors ++ ", " ++
               "'rgb' | '\"r g b\"' where r,g,b are one of {0,1,2,3,4,5}, " ++
               "'time' to chose colors based on server start time. " ++
               "Default is 322 / \"3 2 2\". Incompatible with --serverName."
               )))
      <*> optional
            (option backendArg
              (  long "render"
              <> short 'r'
              <> help (
              "[Client] 'console': play in the console. " ++
              "'opengl': play in an opengl window. When omitted, the player " ++
              "will be asked to chose interactively." ++
              renderHelp)
              ))
      <*> optional
            (option suggestedPlayerName
              (  long "playerName"
              <> help (
              "[Client] the name of the player you want to use. " ++
              "Default is \"Player\".")
              ))
      <*> switch
            (  long "debug"
            <> short 'd'
            <> help
            "[Client] print debug infos in the terminal."
            )

renderHelp :: String
renderHelp =
  "\nAccepted synonyms of 'console' are 'ascii', 'term', 'terminal'." ++
  "\nAccepted synonyms of 'opengl' are 'win', 'window'."

predefinedColor :: String -> Maybe (Color8 Foreground)
predefinedColor = flip Map.lookup predefinedColors

descPredefinedColors :: String
descPredefinedColors =
  "{'" ++
  intercalate "','" listPredefinedColors ++
  "'}"

listPredefinedColors :: [String]
listPredefinedColors = Map.keys predefinedColors

predefinedColors :: Map String (Color8 Foreground)
predefinedColors = Map.fromList
  [ ("blue",     rgb 0 3 5)
  , ("violet",   rgb 1 0 3)
  , ("orange" ,  rgb 4 2 1)
  , ("olive"  ,  rgb 3 3 0)
  , ("reddish" , rgb 3 2 2)
  ]

srvLogsArg :: ReadM ServerLogs
srvLogsArg =
  str >>= \s -> case map toLower s of
    "none"    -> return NoLogs
    "console" -> return ConsoleLogs
    st -> readerError $ "Encountered an invalid server log type:\n\t"
                    ++ show st
                    ++ "\nAccepted render types are 'none' and 'console'."

srvColorSchemeArg :: ReadM ColorScheme
srvColorSchemeArg = map toLower <$> str >>= \lowercase -> do
  let err = readerError $
       "Encountered an invalid color scheme:\n\t" ++
       lowercase ++
       "\nAccepted values are:" ++
       "\n - one of " ++ descPredefinedColors ++
       "\n - 'rgb' | '\"r g b\"' where r,g,b are one of 0,1,2,3,4,5 (pure red is for example 500 / \"5 0 0\")" ++
       "\n - 'time'"
      asRGB l = case catMaybes $ map (readMaybe . (:[])) l of
        [r,g,b] -> either (const err) (return . ColorScheme) $ userRgb r g b
        _ -> err
  maybe
    (case lowercase of
      "time" -> return UseServerStartTime
      l@[_ ,_ ,_]     -> asRGB l
      [r,' ',g,' ',b] -> asRGB [r,g,b]
      _ -> err)
    (return . ColorScheme)
    $ predefinedColor lowercase

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

runWithBackend :: Bool
               -> Maybe ServerName
               -> Maybe ServerPort
               -> Maybe ServerLogs
               -> Maybe ColorScheme
               -> Maybe BackendType
               -> Maybe SuggestedPlayerName
               -> Bool
               -> IO ()
runWithBackend serverOnly maySrvName maySrvPort maySrvLogs mayColorScheme maybeBackend mayPlayerName debug = do
  let printServerArgs = do
        putStrLn $ "| Server-only : " ++ show serverOnly
        putStrLn $ "| Server name : " ++ show maySrvName
        putStrLn $ "| Server port : " ++ show maySrvPort
        putStrLn $ "| Server logs : " ++ show maySrvLogs
        putStrLn $ "| Colorscheme : " ++ show mayColorScheme
      printClientArgs = do
        putStrLn $ "| Client Rendering : " ++ show maybeBackend
        putStrLn $ "| Client Debug     : " ++ show debug
      printBar =
        putStrLn   " ------------- --------------------------"
  printBar >> printServerArgs >> printBar

  when (isJust maySrvName && serverOnly) $
    error "'--serverOnly' conflicts with '--serverName' (these options are mutually exclusive)."

  let srvPort = fromMaybe defaultPort maySrvPort
      srv = mkServer mayColorScheme maySrvLogs maySrvName srvPort
      player = fromMaybe "Player" mayPlayerName
  newEmptyMVar >>= \ready ->
    if serverOnly
      then
        startServerIfLocal srv ready
      else do
        printClientArgs >> printBar
        -- userPicksBackend must run before 'startServerIfLocal' where we install the termination handlers,
        -- because we want the user to be able to stop the program now.
        backend <- maybe userPicksBackend return maybeBackend

        void $ forkIO $ startServerIfLocal srv ready
        readMVar ready -- wait until listening socket is available.
        queues <- startClient player srv
        case backend of
          Console ->
            newConsoleBackend >>= runWith debug queues srv player
          OpenGLWindow ->
            newOpenGLBackend "Hamazed"
              (Coords 12 8) -- will be a command line arg when we have automatic font adaptation
              (Size 600 1400) -- TODO command line arg FullScreen / fixed size
              >>= either error (runWith debug queues srv player)

{-# INLINABLE runWith #-}
runWith :: (PlayerInput a, DeltaRenderBackend a)
        => Bool
        -> ClientQueues
        -> Server
        -> SuggestedPlayerName
        -> a
        -> IO ()
runWith debug queues srv player backend =
  flip withDefaultPolicies backend $ \drawEnv -> do
    sz <- getDiscreteSize backend
    void $ createState sz debug player srv NotConnected >>=
      runStateT (runReaderT loop $ Env drawEnv backend queues)

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
  prod >>= onEvent
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
              else
                --waitKT <- asks waitKeysTimeout

                liftIO $ do
                  let x = maybe
                        (Just <$> getInputEvent server keys)
                        (\t -> getInputEventBefore t server keys)
                          mayTimeLimit

                  withAsync x $ \res -> do
                    let go =
                          case qt of
                            AutomaticFeed -> wait res -- 0% CPU usage while waiting
                            PollOrWaitOnEvents ->
                          -- Using 100 microseconds as minimum interval between consecutive 'pollPlayerEvents'
                          -- seems to be a good trade-off between "CPU usage while waiting" and reactivity.
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
tryGetInputEvent a b =
  atomically $ fmap (Just . Right . SrvEvt)  (readTQueue a)
           <|> fmap (Just . Left) (readTQueue b)
           <|> return Nothing

getInputEvent :: TQueue ServerEvent
              -> TQueue Key
              -> IO (Either Key GenEvent)
getInputEvent a b =
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
      else
        registerDelay (fromIntegral $ toMicros allowed) >>= \timeout ->
          atomically $ fmap (Just . Right . SrvEvt) (readTQueue a)
                   <|> fmap (Just . Left)  (readTQueue b)
                   <|> (return Nothing << check =<< readTVar timeout)
infixr 1 <<
{-# INLINE (<<) #-}
(<<) :: (Monad m) => m b -> m a -> m b
b << a = a >> b
