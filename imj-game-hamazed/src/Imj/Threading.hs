{-# LANGUAGE NoImplicitPrelude #-}

module Imj.Threading
    ( -- * Threads
     {- |
       We use a separate thread to run the game, to be able to catch Ctrl-C related exception,
       and reset console settings before quitting.

       It doesn't seem to always work, maybe we should use
       <http://zguide.zeromq.org/hs:interrupt this approach> instead.
     -}
      runAndWaitForTermination
    , Termination(..)
    , setupCapabilities
    ) where

import           Imj.Prelude
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

-- | Was the thread termination nomal or due to an error?
data Termination = Normal | Abnormal

--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

-- | Runs an IO action in a separate thread, and waits for it to finish,
-- returning its result.
runAndWaitForTermination :: IO () -> IO Termination
runAndWaitForTermination io = do
  --setupCapabilities
  -- launch game thread
  gameThreadTerminated <- myForkIO io
  -- wait for game thread to finish
  readMVar gameThreadTerminated

-- | Sets the number of capabilities to half the number of processors.
-- Not used at the moment since we don't use parallelism too much.
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
handleTerminationCause (Left e) = Prelude.putStrLn ("From game thread:\n" ++ show e) >> return Abnormal
handleTerminationCause _        = return Normal
