{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Game.Hamazed.Loop.Run
      ( run
      ) where

import           Imj.Prelude
import qualified Prelude (putStrLn)

import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Reader.Class(MonadReader)
import           Control.Monad.Reader(runReaderT)
import           Control.Monad.State.Class(MonadState)
import           Control.Monad.State(runStateT, get)
import           System.Info(os)

import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.Env
import           Imj.Game.Hamazed.Loop.Create
import           Imj.Game.Hamazed.Loop.Draw
import           Imj.Game.Hamazed.Loop.Event
import           Imj.Game.Hamazed.Loop.Update
import           Imj.Game.Hamazed.Parameters
import           Imj.Game.Hamazed.State
import           Imj.Graphics.Render
import           Imj.Graphics.Render.Delta
import           Imj.Input.Types
import           Imj.Input.Blocking
import           Imj.Threading
import           Imj.Timing

{- | Runs the Hamazed game.

If your current terminal window is too small, the program will error and
tell you what is the minimum window size to run the game.

The game doesn't run on Windows, because with GHC,
<https://ghc.haskell.org/trac/ghc/ticket/7353 IO operations cannot be interrupted on Windows>.
-}
run :: IO ()
run =
  if os == "mingw32"
    then
      Prelude.putStrLn $ "Windows is not currently supported,"
      ++ " due to this GHC bug: https://ghc.haskell.org/trac/ghc/ticket/7353."
    else
      void doRun

doRun :: IO Termination
doRun =
  runThenRestoreConsoleSettings
    (createEnv >>= \env ->
      runAndWaitForTermination
      $ do
        g <- initialGame
        runStateT (runReaderT loop env) (createState g))

initialGame :: IO Game
initialGame =
  initialGameState initialParameters
    >>= return . Game Configure initialParameters

initialGameState :: GameParameters -> IO GameState
initialGameState params =
  mkInitialState params firstLevel Nothing
    >>= \case
      Left err -> error err
      Right newState -> return newState

{-# INLINABLE loop #-}
loop :: (Render e, MonadState AppState m, MonadReader e m, MonadIO m)
     => m ()
loop =
  get >>= \(AppState game@(Game mode params s) _ _) ->
    case mode of
      Configure -> do
        draw s
        draw' s
        renderToScreen
        liftIO getKeyThenFlush >>= \case
          AlphaNum c ->
            if c == ' '
              then do
                t <- liftIO getSystemTime
                putGame $ startGame t game
              else do
                let newParams = updateFromChar c params
                newState <- liftIO $ initialGameState newParams
                putGame $ Game Configure newParams newState
          _ -> return ()
        loop
      Play ->
        getEvent >>= \evt -> do
          update evt
          onEvent evt
          when (needsRendering evt) $ -- TODO add a Render event that is pushed to a queue
            getRenderable >>= \(gameState, evtStrs) -> do
              draw gameState
              zipWithM_ (\i str -> drawColorStr str $ Coords i 0) [0..] evtStrs
              renderToScreen
          case evt of
            (Interrupt _) -> return ()
            _ -> loop

startGame :: SystemTime -> Game -> Game
startGame t (Game _ params s) =
  Game Play params $ startGameState t s
