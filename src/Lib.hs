{-# LANGUAGE NoImplicitPrelude #-}

module Lib
    ( run
    ) where

import           Imajuscule.Prelude
import qualified Prelude (putStrLn)

import           System.Info(os)

import           Control.Exception( finally )


import           Console( configureConsoleFor
                        , ConsoleConfig(..) )
import           Game( runGameWorker )
import           GameParameters( getGameParameters )
import           Threading( runAndWaitForTermination
                          , Termination(..) )


--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

run :: IO ()
run =
  if os == "mingw32"
    then
      Prelude.putStrLn "Windows OS is not supported yet. Supported OSes are OSX and linux."
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
