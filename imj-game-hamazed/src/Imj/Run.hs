{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Run
    (
    -- * Run Hamazed
      run
    ) where

import           Imj.Prelude
import qualified Prelude (putStrLn)

import           System.Info(os)

import           Control.Monad.Reader(runReaderT)

import           Imj.Env
import           Imj.Game( gameWorker )
import           Imj.Render.Delta
import           Imj.Threading( runAndWaitForTermination, Termination(..) )

-- | Note that Windows is not supported due to <https://ghc.haskell.org/trac/ghc/ticket/7353 this bug>.
--
-- Also, if your current terminal window is too small, the program will error and
-- tell you what is the minimum window size to run the game.
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
    (createEnv >>= runAndWaitForTermination . runReaderT gameWorker)
