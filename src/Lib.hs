module Lib
    ( run
    ) where


import Data.Time ( UTCTime
                 , diffUTCTime
                 , getCurrentTime)
import Data.Time.Clock ()

import System.IO( getChar
                , hSetBuffering
                , hSetEcho
                , BufferMode( NoBuffering )
                , stdin
                , stdout)
import System.Console.ANSI()
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
-- Impure
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
  newState <- updateGame state
  loop newState

printTimer :: GameState -> IO ()
printTimer s = do
  t <- getCurrentTime
  let time = showTimer t s
  putStr time


updateGame :: GameState -> IO GameState
updateGame state = do
  let eraSecond = 1
  mayInput <- timeout (eraSecond * 1000 * 1000) getChar
  case mayInput of
    Just char -> putChar char
    Nothing   -> return ()
  return state
