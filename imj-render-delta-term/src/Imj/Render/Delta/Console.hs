{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_HADDOCK hide #-}

module Imj.Render.Delta.Console
               ( ConsoleConfig(..)
               , configureConsoleFor
               , restoreConsoleSettings
               , runThenRestoreConsoleSettings
               ) where

import           Imj.Prelude

import           Control.Exception( finally )

import           System.Console.Terminal.Size( size , Window(..))
import           System.Console.ANSI( clearScreen, hideCursor
                                    , setSGR, setCursorPosition, showCursor )
import           System.IO( hSetBuffering
                          , hGetBuffering
                          , hSetEcho
                          , BufferMode( .. )
                          , stdin
                          , stdout )


data ConsoleConfig = Gaming | Editing

configureConsoleFor :: ConsoleConfig -> BufferMode -> IO ()
configureConsoleFor config stdoutMode =
  hSetBuffering stdout stdoutMode >>
  case config of
    Gaming  -> do
      hSetEcho stdin False
      hideCursor
      clearScreen -- do not clearFromCursorToScreenEnd with 0 0, so as to keep
                  -- the current console content above the game.
      let requiredInputBuffering = NoBuffering
      initialIb <- hGetBuffering stdin
      hSetBuffering stdin requiredInputBuffering
      ib <- hGetBuffering stdin
      when (ib /= requiredInputBuffering) $
         error $ "input buffering mode "
               ++ show initialIb
               ++ " could not be changed to "
               ++ show requiredInputBuffering
               ++ " instead it is now "
               ++ show ib
    Editing -> do
      hSetEcho stdin True
      showCursor
      -- do not clearFromCursorToScreenEnd, to retain a potential printed exception
      setSGR []
      size >>= maybe (return ()) (\(Window x _) -> setCursorPosition (pred x) 0)
      hSetBuffering stdout LineBuffering


-- Restores stdin, stdout bufferings, unhides the cursor, restores echo for
-- stdin, restores the buffering of stdout to 'LineBuffering'
restoreConsoleSettings :: IO ()
restoreConsoleSettings =
 configureConsoleFor Editing LineBuffering

-- | Helper function to run an action and restore the console settings when it
-- is finished or when an exception was thrown.
runThenRestoreConsoleSettings :: IO a -> IO a
runThenRestoreConsoleSettings action =
  -- When Ctrl+C is hit, an exception is thrown on the main thread, hence
  -- I use 'finally' to reset the console settings.
  action `finally` restoreConsoleSettings
