{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Imj.Game.Hamazed.Loop.Run
      ( run
      ) where

import           Imj.Prelude
import           Prelude (putStrLn, getChar)

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)
import           Control.Monad.Reader(runReaderT)
import           Control.Monad.State.Class(MonadState)
import           Control.Monad.State(runStateT)
import           Data.Text(pack, toLower)
import           Options.Applicative
                  (progDesc, fullDesc, info, customExecParser, (<**>), prefs, helper
                  , showHelpOnError, short, long, option, str, help, optional, ReadM, readerError)
import           System.Info(os)
import           System.IO(hFlush, stdout)

import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.Env
import           Imj.Game.Hamazed.Level
import           Imj.Game.Hamazed.Loop.Create
import           Imj.Game.Hamazed.Loop.Draw
import           Imj.Game.Hamazed.Loop.Deadlines
import           Imj.Game.Hamazed.Loop.Event
import           Imj.Game.Hamazed.Loop.Event.Priorities
import           Imj.Game.Hamazed.Loop.Update
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Game.Hamazed.Parameters
import           Imj.Game.Hamazed.State
import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.Render
import           Imj.Graphics.Render.Delta
import           Imj.Graphics.Render.FromMonadReader
import           Imj.Graphics.UI.RectContainer
import           Imj.Input.Types
import           Imj.Input.FromMonadReader
import           Imj.Timing

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
      argsPickBackend >>= (\case
        Nothing -> userPicksBackend
        Just x -> return x) >>= runWithBackend

data BackendType = Console
                 | OpenGLWindow

backendArg :: ReadM BackendType
backendArg = str >>= \s -> case toLower $ pack s of
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

renderHelp :: String
renderHelp =
  "\nAccepted synonyms of 'console' are 'ascii', 'term', 'terminal'." ++
  "\nAccepted synonyms of 'opengl' are 'win', 'window'."

argsPickBackend :: IO (Maybe BackendType)
argsPickBackend =
  customExecParser p opts
  where
    opts = info (optional
                  $ option backendArg (long "render"
                                    <> short 'r'
                                    <> help ("Use argument 'console' to play in the console. Use 'opengl' to play in an opengl window (experimental). " ++ renderHelp))
                <**> helper)
      ( fullDesc
     <> progDesc "imj-game-hamazed-exe runs the 'Hamazed' game." )
    p = prefs showHelpOnError

userPicksBackend :: IO BackendType
userPicksBackend = do
  putStrLn ""
  putStrLn " Welcome to Hamazed!"
  putStrLn ""
  putStrLn " - Press (1) then (Enter) to play in the console, where the game will be rendered using colored ascii characters."
  putStrLn "     An error message will inform you if your console is too small."
  putStrLn "          [Equivalent to '-r console']"
  putStrLn " - Press (2) then (Enter) to play in a separate window where openGL will be used to render the game (experimental)."
  putStrLn "          [Equivalent to '-r opengl']"
  putStrLn ""
  hFlush stdout
  getChar >>= \case
    '1' -> return Console
    '2' -> return OpenGLWindow
    _ -> userPicksBackend

runWithBackend :: BackendType -> IO ()
runWithBackend = \case
  Console      -> newConsoleBackend >>= runWith
  OpenGLWindow -> newOpenGLBackend "Hamazed" >>= runWith

{-# INLINABLE runWith #-}
runWith :: (PlayerInput a, DeltaRenderBackend a)
        => a -> IO ()
runWith backend =
  -- TODO simplify, backend is used 4 times!
  withDefaultPolicies (\drawEnv -> do
    sz <- getDiscreteSize backend
    void (createState sz
      >>= runStateT (runReaderT loop (Env drawEnv backend)))) backend

{-# INLINABLE loop #-}
loop :: (Render e, MonadState AppState m, PlayerInput e, MonadReader e m, MonadIO m)
     => m ()
loop = do
  playerEndsProgram >>= \end -> unless end $
   getGame >>= \(Game mode params s@(GameState _ (World _ _ (Space _ sz _) _) _ _ _ _ _)) ->
    case mode of
      Configure -> do
        (Screen _ centerScreen) <- getCurScreen
        draw s
        draw' $ mkRectContainerWithCenterAndInnerSize centerScreen sz
        renderToScreen
        getPlayerKey >>= \case
          AlphaNum ' ' -> do
                kt <- KeyTime <$> liftIO getSystemTime
                putGame $ Game Play params $ startGameState kt s
          AlphaNum c -> do
                let newParams = updateFromChar c params
                getTargetSize
                  >>= liftIO . initialGameState newParams
                  >>= putGame . Game Configure newParams
          _ -> return ()
        loop
      Play -> do
        let renderAll =
              getRenderable >>= \(gameState, evtStrs) -> do
                draw gameState
                zipWithM_ (\i evtStr -> drawAt evtStr $ Coords i 0) [0..] evtStrs
                renderToScreen
            printStats _x = return ()-- liftIO $ putStrLn $ replicate (pred _x) ' ' ++ "|"
        playLoop >>= maybe
          (return ())
          (\n -> printStats n >> renderAll >> loop)


{-# INLINABLE playLoop #-}
playLoop :: (MonadState AppState m, PlayerInput e, MonadReader e m, MonadIO m)
         => m (Maybe Int)
playLoop = do
  timeBegin <- liftIO getSystemTime
  let go n principal visibles = do
        playerPending <- hasPlayerKey
        let onDeadline d@(Deadline (KeyTime deadlineTime) priority _)
             |preferPlayer && playerPending = tryPlayer -- won't block
             |deadlineIsOverdue = tryUpdate $ Timeout d
             |otherwise =
               -- deadline is not overdue
               if null visibles
                 then
                   -- we have no update that needs to be rendered, hence we can wait
                   getEventForDeadline d >>= maybe (return $ Just n) tryUpdate
                 else
                   -- we have updates that should be rendered
                   return $ Just n
             where
              preferPlayer = not deadlineIsOverdue || priority < playerPriority
              deadlineIsOverdue = deadlineTime < timeBegin

            deadlineStarvation =
              if null visibles
                then
                  tryPlayer -- may block, waiting for player input
                else
                  return $ Just n

            tryPlayer =
              getPlayerKey >>= \k -> eventFromKey' k >>= \case
                Nothing -> return $ Just n
                Just e -> tryUpdate e >>= \res -> case res of
                  -- unGet the 'Key' if the corresponding update was not performed:
                  Just n' -> when (n == n') (unGetPlayerKey k) >> return res
                  Nothing -> return res

            tryUpdate (Interrupt _) = return Nothing
            tryUpdate e
             |conflict  = return $ Just n
             |otherwise = update e >> onEvent e >> go'
             where
              conflict = thisPrincipal && principal
              !thisPrincipal = isPrincipal e
              go' = go (succ n) (thisPrincipal || principal) newVisibles
              newVisibles = [e | isVisible e] ++ visibles
        getNextDeadline timeBegin >>= maybe deadlineStarvation onDeadline
  go 0 False []
