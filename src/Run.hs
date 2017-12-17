{-# LANGUAGE NoImplicitPrelude #-}

module Run
    ( run
    ) where

import           Imajuscule.Prelude
import qualified Prelude (putStrLn)

import           System.Info(os)

import           Control.Exception( finally )
import           Control.Monad.Reader(runReaderT, ReaderT)

import           Draw
import           Env
import           Game( runGameWorker )
import           Game.Parameters( getGameParameters )
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
  (createEnv >>= runAndWaitForTermination . runReaderT gameWorker)
  -- When Ctrl+C is hit, an exception is thrown on the main thread, hence
  -- I use 'finally' to reset the console settings.
  `finally` restoreConsole

{-# INLINABLE gameWorker #-}
gameWorker :: (Draw e) => ReaderT e IO ()
gameWorker =
  getGameParameters >>= runGameWorker
