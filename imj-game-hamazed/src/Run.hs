{-# LANGUAGE NoImplicitPrelude #-}

module Run
    ( run
    ) where

import           Imajuscule.Prelude
import qualified Prelude (putStrLn)

import           System.Info(os)

import           Control.Monad.Reader(runReaderT)

import           Env
import           Game( gameWorker )
import           Render.Delta
import           Threading( runAndWaitForTermination, Termination(..) )

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
