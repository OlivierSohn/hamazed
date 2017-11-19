{-# LANGUAGE NoImplicitPrelude #-}

module Threading
    ( runAndWaitForTermination
    , Termination(..)
    , setupCapabilities
    ) where

import           Imajuscule.Prelude
import qualified Prelude

import           GHC.Conc(getNumProcessors)
import           Control.Concurrent( forkFinally
                                   , MVar
                                   , newEmptyMVar
                                   , putMVar
                                   , readMVar
                                   , setNumCapabilities )
import           Control.Exception( SomeException(..) )
import           Control.Monad( (>=>) )


data Termination = Normal | Abnormal

--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

runAndWaitForTermination :: IO () -> IO Termination
runAndWaitForTermination io = do
  --setupCapabilities
  -- launch game thread
  gameThreadTerminated <- myForkIO io
  -- wait for game thread to finish
  readMVar gameThreadTerminated

-- TODO is it needed?
setupCapabilities :: IO ()
setupCapabilities = do
  nproc <- getNumProcessors
  let ncap = max 1 $ quot nproc 2
  setNumCapabilities ncap


-- This function was introduced so that the parent thread can wait on the
-- returned MVar to be set to know that the child thread has terminated.
-- cf https://hackage.haskell.org/package/base-4.10.0.0/docs/Control-Concurrent.html#g:12
myForkIO :: IO () -> IO (MVar Termination)
myForkIO io = do
  mvar <- newEmptyMVar
  _ <- forkFinally io (handleTerminationCause >=> putMVar mvar)
  return mvar


handleTerminationCause :: Either SomeException a -> IO Termination
handleTerminationCause (Left e) = Prelude.putStrLn ("Game thread terminates abnormaly: " ++ show e) >> return Abnormal
handleTerminationCause _        = return Normal
