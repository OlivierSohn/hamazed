module Threading
    ( runAndWaitForTermination
    ) where


import Control.Concurrent( forkFinally
                         , MVar
                         , newEmptyMVar
                         , putMVar
                         , readMVar )
import Control.Exception( SomeException(..) )


--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

runAndWaitForTermination :: IO () -> IO ()
runAndWaitForTermination io = do
  -- launch game thread
  gameThreadTerminated <- myForkIO io
  -- wait for game thread to finish
  readMVar gameThreadTerminated


-- This function was introduced so that the parent thread can wait on the
-- returned MVar to be set to know that the child thread has terminated.
-- cf https://hackage.haskell.org/package/base-4.10.0.0/docs/Control-Concurrent.html#g:12
myForkIO :: IO () -> IO (MVar ())
myForkIO io = do
  mvar <- newEmptyMVar
  _ <- forkFinally io (\e -> handleTerminationCause e >> putMVar mvar ())
  return mvar


handleTerminationCause :: Either SomeException a -> IO()
handleTerminationCause (Left e) = putStrLn $ "Game thread terminates abnormaly: " ++ show e
handleTerminationCause _        = return ()
