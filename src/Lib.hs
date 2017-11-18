module Lib
    ( run
    ) where

import           Imajuscule.Prelude

import           Control.Exception( finally )


import           Console( configureConsoleFor
                        , ConsoleConfig(..) )
import           Game( gameWorker )
import           Threading( runAndWaitForTermination
                          , Termination(..) )


--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

run :: IO Termination
run =
  (configureConsoleFor Gaming >> runAndWaitForTermination gameWorker)
  -- When Ctrl+C is hit, an exception is thrown on the main thread, hence
  -- I use 'finally' to reset the console settings.
  `finally`
   configureConsoleFor Editing
