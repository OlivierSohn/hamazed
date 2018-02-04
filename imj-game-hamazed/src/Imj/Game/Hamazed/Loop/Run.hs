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
import           Prelude (putStrLn, getLine)

import           Control.Distributed.Process.Internal.Types
import           Control.Distributed.Process.Lifted.Class(MonadProcess)
import           Control.Distributed.Process.Node(runProcess, initRemoteTable, newLocalNode)
import           Control.Distributed.Process(newChan)
import           Control.Exception(throwIO)
import           Control.Monad(join)
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)
import           Control.Monad.Reader(runReaderT)
import           Control.Monad.State.Class(MonadState)
import           Control.Monad.State(runStateT)
import           Data.Text(pack, toLower)
import           Network.Transport(address)
import           Network.Transport.TCP(defaultTCPParameters, createTransport)
import           Options.Applicative
                  (progDesc, fullDesc, info, header, customExecParser, prefs, helper
                  , showHelpOnError, short, long, option, str, help, optional
                  , ReadM, readerError, (<*>), switch)
import           System.Info(os)
import           System.IO(hFlush, stdout)

import           Imj.Game.Hamazed.Env
import           Imj.Game.Hamazed.KeysMaps
import           Imj.Game.Hamazed.Loop.Deadlines
import           Imj.Game.Hamazed.Loop.Event
import           Imj.Game.Hamazed.Server
import           Imj.Game.Hamazed.State
import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Render
import           Imj.Graphics.Render.Delta
import           Imj.Input.Types
import           Imj.Input.FromMonadReader

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

data BackendType = Console
                 | OpenGLWindow

runWithArgs :: IO ()
runWithArgs =
  join . customExecParser (prefs showHelpOnError) $
    info (helper <*> parser)
    (  fullDesc
    <> header "imj-game-hamazed-exe runs the 'Hamazed' game."
    <> progDesc "Hamazed is a game with flying numbers abd 8-bit color animations."
    )
 where
  parser =
    runWithBackend
      <$> optional
             (option backendArg (long "render"
                              <> short 'r'
                              <> help ("Use argument 'console' to play in the console. " ++
                                        "Use 'opengl' to play in an opengl window. " ++
                                        renderHelp)))
      <*> switch ( long "debug" <> short 'd' <> help "Print debug infos in the terminal." )

renderHelp :: String
renderHelp =
  "\nAccepted synonyms of 'console' are 'ascii', 'term', 'terminal'." ++
  "\nAccepted synonyms of 'opengl' are 'win', 'window'."

backendArg :: ReadM BackendType
backendArg =
  str >>= \s -> case toLower $ pack s of
    "ascii"        -> return Console
    "console"      -> return Console
    "term"         -> return Console
    "terminal"     -> return Console
    "opengl"       -> return OpenGLWindow
    "win"          -> return OpenGLWindow
    "window"       -> return OpenGLWindow
    _ -> readerError $ "encountered an invalid render type:\n\t"
                    ++ show s
                    ++ "\nAccepted render types are 'console' and 'opengl'."
                    ++ renderHelp

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
  hFlush stdout -- just in case buffer mode is block
  getLine >>= \case
    "1" -> return Console
    "2" -> return OpenGLWindow
    c -> putStrLn ("invalid value : " ++ c) >> userPicksBackend

runWithBackend :: Maybe BackendType -> Bool -> IO ()
runWithBackend maybeBackend debug =
  maybe userPicksBackend return maybeBackend >>= \case
    Console      -> newConsoleBackend >>= runWith debug
    OpenGLWindow -> newOpenGLBackend "Hamazed" 10 (Size 600 1400) >>= runWith debug

{-# INLINABLE runWith #-}
runWith :: (PlayerInput a, DeltaRenderBackend a)
        => Bool -> a -> IO ()
runWith debug backend =
  flip withDefaultPolicies backend (\drawEnv -> do
    let host = "127.0.0.1"
    createTransport host "10501" (\sn -> (host, sn)) defaultTCPParameters >>= either
      throwIO
      (\t -> newLocalNode t initRemoteTable >>= \ln -> runProcess ln (do
        --liftIO $ putStrLn $ "Echo server started at " ++ show (address $ localEndPoint ln)
        (s,r) <- newChan
        let env = Env drawEnv backend $ ServerEvtChan s r
        sz <- getDiscreteSize backend
        state <- liftIO $ createState sz debug
        let actState = void (runStateT (runReaderT loop env) state)
        runReaderT actState env)))

loop :: (MonadState AppState m, MonadProcess m, MonadReader e m, ClientNode e, Render e, PlayerInput e)
     => m ()
loop = do
  produceEvent >>= \case
    (Just (Left (Interrupt _ ))) -> return ()
    mayEvt -> playerEndsProgram >>= \case
      True -> return ()
      _ -> onEvent mayEvt >> loop

-- | MonadState AppState is needed to know if the level is finished or not.
{-# INLINABLE produceEvent #-}
produceEvent :: (MonadState AppState m, MonadReader e m, PlayerInput e, MonadIO m)
             => m (Maybe (Either Event ClientEvent))
produceEvent =
  -- 'playerPriority' is bigger that every other priority so we handle non-blocking player events:
  hasPlayerKey >>= \case
    True -> getPlayerKey >>= eventFromKey
    False -> do
      let whenWaitingIsAllowed x = hasVisibleNonRenderedUpdates >>= \case
            True ->
              -- we can't afford waiting, we force a render
              return Nothing
            False ->
              -- we can afford waiting, we execute the action
              x
      getLastRenderTime >>= getNextDeadline >>= maybe
          (whenWaitingIsAllowed $ getPlayerKey >>= eventFromKey)
          (\case
            Overdue d -> return $ Just $ Left $ Timeout d
            Future d@(Deadline deadlineTime _ _) ->
              whenWaitingIsAllowed $ do
                getPlayerKeyBefore deadlineTime >>= \case
                  Just key -> eventFromKey key
                  Nothing -> return $ Just $ Left $ Timeout d)
