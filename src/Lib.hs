module Lib
    ( run
    ) where


import Control.Concurrent( forkFinally
                         , MVar
                         , newEmptyMVar
                         , putMVar
                         , readMVar )
import Control.Exception( ArithException(..)
                        , finally
                        , SomeException(..)
                        , throw )
import Control.Monad( when )
import Data.Time( UTCTime
                , diffUTCTime
                , getCurrentTime )
import System.Console.ANSI( clearScreen
                          , hideCursor
                          , setCursorPosition
                          , showCursor )
import System.IO( getChar
                , hSetBuffering
                , hSetEcho
                , BufferMode( NoBuffering, LineBuffering )
                , stdin
                , stdout )
import System.Timeout( timeout )


--------------------------------------------------------------------------------
-- Pure
--------------------------------------------------------------------------------

data GameState = GameState {
    _startTime :: !UTCTime
}

data ConsoleConfig = Game | Edition


showTimer :: UTCTime -> GameState -> String
showTimer currentTime (GameState startTime) =
  let delta = diffUTCTime currentTime startTime
  in show (round delta :: Integer)


--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

run :: IO ()
run =
  (setupConsoleFor Game >> runGameAndWaitForTermination)
  -- When Ctrl+C is hit, an exception is thrown on the main thread, hence
  -- I use 'finally' to reset the console settings.
  `finally`
   setupConsoleFor Edition


runGameAndWaitForTermination :: IO ()
runGameAndWaitForTermination = do
  -- launch game thread
  gameThreadTerminated <- myForkIO gameThread
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


gameThread :: IO ()
gameThread = makeInitialState >>= loop


setupConsoleFor :: ConsoleConfig -> IO ()
setupConsoleFor config = do
  hSetEcho stdin $ case config of
      Game    -> False
      Edition -> True
  hSetBuffering stdout $ case config of
      Game    -> NoBuffering
      Edition -> LineBuffering
  case config of
    Game    -> hideCursor >> clearScreen
    Edition -> showCursor  -- do not clearScreen, to retain a potential printed exception


makeInitialState :: IO GameState
makeInitialState = do
  t <- getCurrentTime
  return $ GameState t


loop :: GameState -> IO ()
loop state = do
  setCursorPosition 0 0
  printTimer state
  updateGame state >>= loop


printTimer :: GameState -> IO ()
printTimer s = do
  t <- getCurrentTime
  putStr $ showTimer t s


-- Game update:
-- Wait one second for a key to be pressed. If timeout, return.
-- Print the pressed key.
-- If the 'o' key was pressed, throw an overflow exception.
updateGame :: GameState -> IO GameState
updateGame state = do
  let eraSecond = 1
  mayInput <- timeout (eraSecond * 1000 * 1000) getChar
  -- TODO try to chain putChar with the lambda after
  mapM_ putChar mayInput
  mapM_ (\c -> when (c == 'o') (do
    putStrLn ""
    putStrLn $ "You lost! The '" ++ [c] ++ "' key throws an overflow exception in the game thread."
    throw Overflow)) mayInput
  return state
