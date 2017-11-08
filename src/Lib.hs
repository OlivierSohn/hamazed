
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
import           System.Console.ANSI( setCursorPosition
                                    , clearScreen )
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


actionFromChar :: Char -> Maybe Action
actionFromChar c = case c of
  'o' -> Just Throw
  's' -> Just $ Frame Down
  'w' -> Just $ Frame Up
  'a' -> Just $ Frame Left
  'd' -> Just $ Frame Right
  _   -> Nothing


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


nextUpdateCounter :: Int -> Int
nextUpdateCounter c = (c + 1) `mod` maxUpdateTick


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


updateGame :: GameState -> RenderState -> IO GameState
updateGame s r = do
  clearScreen
  newGameState <- renderGame s r
  hFlush stdout
  return newGameState


renderGame :: GameState -> RenderState -> IO GameState
renderGame state@(GameState t c frameCorner) (RenderState renderCorner) = do

  let eraMillis = 160 -- this controls the game loop frequency.
                      -- 20 seems to match screen refresh frequency
      eraMicros = eraMillis * 1000
  maybeAction <- timeout eraMicros getChar >>= mapM (return . actionFromChar)

  let frameOffset = case maybeAction of
        (Just (Just (Frame a))) -> coordsForDirection a
        _ -> zeroCoords
      r = RenderState $ sumCoords renderCorner frameOffset

  r2 <- printTimer state r

  _ <- case maybeAction of
    (Just (Just Throw)) -> do
      _ <- putStrLn r2 "Boom! An overflow exception was thrown in the game thread."
      throw Overflow
    _ -> return ()

  return $ GameState t (nextUpdateCounter c) (sumCoords frameCorner frameOffset)


putStrLn :: RenderState -> String -> IO RenderState
putStrLn (RenderState (Coords (Row r) (Col c))) str = do
  setCursorPosition r c
  Prelude.putStrLn str
  return $ RenderState $ Coords (Row $ r + 1) (Col c)
