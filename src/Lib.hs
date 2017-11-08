module Lib
    ( run
    ) where


import Data.Time ( UTCTime
                 , diffUTCTime
                 , getCurrentTime)
import System.Console.ANSI()
import System.IO( getChar
                , hSetBuffering
                , hSetEcho
                , BufferMode( NoBuffering )
                , stdin
                , stdout)
import System.Timeout (timeout)


--------------------------------------------------------------------------------
-- Pure
--------------------------------------------------------------------------------

data GameState = GameState {
    _startTime :: !UTCTime
}


showTimer :: UTCTime -> GameState -> String
showTimer currentTime (GameState startTime) =
  let delta = diffUTCTime currentTime startTime
  in show (round delta :: Integer)


--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

run :: IO ()
run = configure >> makeInitialState >>= loop


configure :: IO ()
configure = do
  -- we don't want the result of `getChar` to be echoed
  hSetEcho stdin False

  -- we want the console to update immediately
  -- TODO will this be still needed once we use more complex stuff ?
  hSetBuffering stdout NoBuffering


makeInitialState :: IO GameState
makeInitialState = do
  t <- getCurrentTime
  return $ GameState t


loop :: GameState -> IO ()
loop state = do
  printTimer state
  updateGame state >>= loop


printTimer :: GameState -> IO ()
printTimer s = do
  t <- getCurrentTime
  putStr $ showTimer t s


updateGame :: GameState -> IO GameState
updateGame state = do
  let eraSecond = 1
  mayInput <- timeout (eraSecond * 1000 * 1000) getChar
  mapM_ putChar mayInput
  return state
