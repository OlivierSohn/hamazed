{-# LANGUAGE NoImplicitPrelude #-}

module Run
    ( run
    ) where

import           Imajuscule.Prelude
import qualified Prelude (putStrLn)

import           System.Info(os)

import           Control.Exception( finally )


import           Game( runGameWorker )
import           Game.Parameters( getGameParameters )
import           Render.Console( configureConsoleFor, ConsoleConfig(..) )
import           Threading( runAndWaitForTermination, Termination(..) )


--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

run :: IO ()
run =
  if os == "mingw32"
    then
      Prelude.putStrLn "Windows is not currently supported, due to this GHC bug: https://ghc.haskell.org/trac/ghc/ticket/7353."
    else
      void doRun

doRun :: IO Termination
doRun =
  (configureConsoleFor Gaming >> runAndWaitForTermination gameWorker)
  -- When Ctrl+C is hit, an exception is thrown on the main thread, hence
  -- I use 'finally' to reset the console settings.
  `finally`
   configureConsoleFor Editing

gameWorker :: IO ()
gameWorker = getGameParameters >>= runGameWorker
