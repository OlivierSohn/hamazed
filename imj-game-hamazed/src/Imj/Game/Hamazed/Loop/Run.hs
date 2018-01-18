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
import           Control.Monad.State(runStateT, get)
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
  end <- playerEndsProgram
  unless end $
   get >>= \(AppState game@(Game mode params s@(GameState _ (World _ _ (Space _ sz _) _) _ _ _ _ _)) _ _) ->
    case mode of
      Configure -> do
        (Screen _ centerScreen) <- getCurScreen
        draw s
        draw' $ mkRectContainerWithCenterAndInnerSize centerScreen sz
        renderToScreen
        getPlayerKey >>= \case
          AlphaNum c ->
            if c == ' '
              then do
                t <- liftIO getSystemTime
                putGame $ startGame t game
              else do
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

-- TODO when there are a lot of overdue deadlines, we need to limit
-- the work we do by computing updates for a limited time :
-- maxCumulUpdates = floatSecondsToDiffTime 0.008
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
              | conflict  = return $ Just n
              | otherwise = update e >> onEvent e >> go'
              where
                conflict = thisPrincipal && principal
                !thisPrincipal = isPrincipal e
                go' = go (succ n) (thisPrincipal || principal) newVisibles
                newVisibles = [e | isVisible e] ++ visibles
        getNextDeadline timeBegin >>= maybe deadlineStarvation onDeadline
  go 0 False []

  {- TODO measure time differences using the monotonic clock in
  https://hackage.haskell.org/package/clock-0.4.1.3/docs/System-Clock.html
  It will measure real time differences (not cpu time differences) and will not be
  subject to time adjustment of system time-of-day. -}

  {- Deadlines are ordered on a timeline, and are associated to events.

  Deadlines are prioritized. A lower priority deadline can be run after a higher priority deadline, even if
  it is placed before it on the timeline.

  Some events don't induce visible changes, hence they don't call for a subsequent rendering.
  One example is when the user wants the ship to accelerate, it changes the ship velocity but not
  its position.

  Some events do call for a subsequent rendering. The will be called "visible" deadlines.
  If we render after each of them, we will run out of CPU / GPU when they are close to one another.
  Hence, we want to group those events before doing a render.

  The time interval inside which deadlines can be handled in one iteration
  is : [-infinity, t+0.01], where t is:
  - the current time, if there are visible overdue deadlines w.r.t the current time
  (meaning a previous update / render / ... was too long)
  - else (we wait for potential user input in that case) : the time
  at which we handle the update of the closest deadline with visible change, which is either the
  foreseen deadline time or the time at which a user input was received.

  All deadlines within this interval will be handled within the same iteration if the updates
  are fast, but as soon as the cumulated update duration becomes greater than 0.008,
  we force a render. Hence, the order in which the deadlines are updated is important,
  and defined this way: we first handle all overdue deadlines, then we handle the
  closest deadlines.

  To keep things consistent for the user, we should not group two visible deadlines
  that modify the game state in the same step.
  -}

startGame :: SystemTime -> Game -> Game
startGame t (Game _ params s) =
  Game Play params $ startGameState t s
