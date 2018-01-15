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
      doRun

doRun :: IO ()
doRun =
--  newConsoleBackend
  newOpenGLBackend "Hamazed"
  -- TODO simplify, backend is used 4 times!
    >>= \backend -> withDefaultPolicies (\drawEnv -> do
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
      Play ->
        getEvent >>= \evt -> do
          update evt
          onEvent evt
          when (needsRendering evt) $ -- TODO add a Render event that is pushed to a queue
            getRenderable >>= \(gameState, evtStrs) -> do
              draw gameState
              zipWithM_ (\i str -> drawAt str $ Coords i 0) [0..] evtStrs
              renderToScreen
          case evt of
            (Interrupt _) -> return ()
            _ -> loop

startGame :: SystemTime -> Game -> Game
startGame t (Game _ params s) =
  Game Play params $ startGameState t s
