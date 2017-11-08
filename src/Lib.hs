module Lib
    ( run
    ) where


import Control.Exception( ArithException(..)
                        , finally
                        , throw )
import Control.Monad( when )
import Data.Time( UTCTime
                , diffUTCTime
                , getCurrentTime )
import System.Console.ANSI( setCursorPosition )
import System.IO( getChar
                , hFlush
                , stdout )
import System.Timeout( timeout )


import Console( configureConsoleFor
              , ConsoleConfig(..) )
import Threading( runAndWaitForTermination )

--------------------------------------------------------------------------------
-- Pure
--------------------------------------------------------------------------------

data GameState = GameState {
    _startTime :: !UTCTime
  , _updateCounter :: Int
}


maxUpdateTick :: Int
maxUpdateTick = 10


tickRepresentationLength :: Int
tickRepresentationLength = quot maxUpdateTick 2


showUpdateTick :: Int -> String
showUpdateTick t =
  let nDotsBefore = max 0 (t + tickRepresentationLength - maxUpdateTick)
      nLeftBlanks = t - nDotsBefore
      nDotsAfter = tickRepresentationLength - nDotsBefore
      nRightBlanks = maxUpdateTick - t - tickRepresentationLength
  in replicate nDotsBefore  '.'
  ++ replicate nLeftBlanks  ' '
  ++ replicate nDotsAfter   '.'
  ++ replicate nRightBlanks ' '

showTimer :: UTCTime -> GameState -> String
showTimer currentTime (GameState startTime updateTick) =
  let timeExact = diffUTCTime currentTime startTime
      time = floor timeExact :: Integer
  in "|" ++ showUpdateTick updateTick ++ "| " ++ show time ++ " |"


--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

run :: IO ()
run =
  (configureConsoleFor Gaming >> runAndWaitForTermination gameWorker)
  -- When Ctrl+C is hit, an exception is thrown on the main thread, hence
  -- I use 'finally' to reset the console settings.
  `finally`
   configureConsoleFor Editing


gameWorker :: IO ()
gameWorker = makeInitialState >>= loop


makeInitialState :: IO GameState
makeInitialState = do
  t <- getCurrentTime
  return $ GameState t 0


loop :: GameState -> IO ()
loop state = do
  setCursorPosition 0 0
  printTimer state
  updateGame state >>= loop


printTimer :: GameState -> IO ()
printTimer s = do
  t <- getCurrentTime
  putStrLn $ showTimer t s


-- Game update:
-- Wait one second for a key to be pressed. If timeout, return.
-- Print the pressed key.
-- If the 'o' key was pressed, throw an overflow exception.
updateGame :: GameState -> IO GameState
updateGame (GameState t updateCounter) = do
  let eraMilliSeconds = 160 -- this controls the game loop frequency.
                           -- 20 seems to match screen refresh frewquency
  mayInput <- timeout (eraMilliSeconds * 1000) getChar
  mapM_ (\c -> putStrLn [c] >> when (c == 'o') (do
    putStrLn $ "Boom! The '" ++ [c] ++ "' key throws an overflow exception in the game thread."
    throw Overflow)) mayInput
  hFlush stdout
  return $ GameState t $ (updateCounter + 1) `mod` maxUpdateTick
