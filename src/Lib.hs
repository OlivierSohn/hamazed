{-# LANGUAGE LambdaCase #-}

module Lib
    ( run
    ) where


import           Prelude hiding ( Left
                                , Right
                                , putStrLn )
import qualified Prelude (putStrLn)


import           Control.Exception( ArithException(..)
                                  , finally
                                  , throw )
import           Data.Time( UTCTime
                          , diffUTCTime
                          , getCurrentTime )
import           Data.Maybe( fromMaybe )
import           System.Console.ANSI( setCursorPosition )
import           System.IO( getChar
                          , hFlush
                          , stdout )
import           System.Timeout( timeout )


import           Console( configureConsoleFor
                        , ConsoleConfig(..) )
import           Geo( sumCoords
                    , coordsForDirection
                    , Col(..)
                    , Coords(..)
                    , Direction(..)
                    , Row(..) )
import           Threading( runAndWaitForTermination )

--------------------------------------------------------------------------------
-- Pure
--------------------------------------------------------------------------------

data Action = Frame Direction | Throw


data GameState = GameState {
    _startTime :: !UTCTime
  , _updateCounter :: !Int
  , _upperLeftCorner :: !Coords
}

newtype RenderState = RenderState {
    _currentUpperLeftCorner :: Coords
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
showTimer currentTime (GameState startTime updateTick _) =
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


zeroCoords :: Coords
zeroCoords = Coords (Row 0) (Col 0)


makeInitialState :: IO GameState
makeInitialState = do
  t <- getCurrentTime
  return $ GameState t 0 zeroCoords


loop :: GameState -> IO ()
loop state@(GameState _ _ coords@(Coords (Row row) (Col col))) = do
  setCursorPosition row col
  let r = RenderState coords
  updateGame state r >>= loop


printTimer :: GameState -> RenderState -> IO RenderState
printTimer s r = do
  t <- getCurrentTime
  putStrLn r $ showTimer t s


-- Game update:
-- Wait one second for a key to be pressed. If timeout, return.
-- Print the pressed key.
-- If the 'o' key was pressed, throw an overflow exception.
updateGame :: GameState -> RenderState -> IO GameState
updateGame state@(GameState t updateCounter oldCoords) (RenderState rCoords) = do
  let eraMilliSeconds = 160 -- this controls the game loop frequency.
                           -- 20 seems to match screen refresh frewquency
  mayInput <- timeout (eraMilliSeconds * 1000) getChar
  action <- mapM (return . (\case
    'o' -> Just Throw
    's' -> Just $ Frame Down
    'w' -> Just $ Frame Up
    'a' -> Just $ Frame Left
    'd' -> Just $ Frame Right
    _   -> Nothing
      )) mayInput

  let newUpdateCounter = (updateCounter + 1) `mod` maxUpdateTick
      offsetCoords = case action of
        (Just (Just (Frame a))) -> coordsForDirection a
        _ -> zeroCoords

  r2 <- printTimer state (RenderState $ sumCoords rCoords offsetCoords)
  r3_ <- mapM (\c -> putStrLn r2 [c]) mayInput
  let r3 = fromMaybe r2 r3_
  _ <- case action of
    (Just (Just Throw)) -> do
      _ <- putStrLn r3 "Boom! An overflow exception was thrown in the game thread."
      throw Overflow
    _ -> return ()

  hFlush stdout
  return $ GameState t newUpdateCounter $ sumCoords oldCoords offsetCoords


putStrLn :: RenderState -> String -> IO RenderState
putStrLn (RenderState (Coords (Row r) (Col c))) str = do
  setCursorPosition r c
  Prelude.putStrLn str
  return $ RenderState $ Coords (Row $ r + 1) (Col c)
