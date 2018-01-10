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
import           Control.Monad.State(runStateT, state, get)
import           System.Info(os)

import           Imj.Game.Hamazed.Env
import           Imj.Game.Hamazed.Loop.Create
import           Imj.Game.Hamazed.Loop.Draw
import           Imj.Game.Hamazed.Loop.Event
import           Imj.Game.Hamazed.Loop.Update
import           Imj.Game.Hamazed.Parameters
import           Imj.Game.Hamazed.State
import           Imj.Game.Hamazed.Types
import           Imj.Graphics.Render
import           Imj.Graphics.Render.Delta
import           Imj.Threading

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
      $ runStateT (runReaderT loop env) createState)

{-# INLINABLE loop #-}
loop :: (Render e, MonadState AppState m, MonadReader e m, MonadIO m)
     => m ()
loop =
  get >>= \(AppState game _ _) ->
    case game of
      Nothing -> do
        params <- getGameParameters
        mkInitialState params firstLevel Nothing
          >>= \case
            Left err -> error err
            Right newState -> state (setGame $ Just (Game params newState)) >> loop
      Just (Game params st) ->
        liftIO (getEvent st) >>= \evt -> do
          newState <- liftIO $ update params st evt
          state (setGame $ Just (Game params newState))
          strEvt <- onEvent evt
          when (needsRendering evt) $ do
            draw newState
            zipWithM_ (\i s -> drawColorStr s $ Coords i 0) [0..] strEvt
            renderToScreen
          case evt of
            (Interrupt _) -> return ()
            _ -> loop
