
module Lib
    ( run
    ) where


import           Prelude hiding ( Left
                                , Right )


import           Control.Exception( ArithException(..)
                                  , finally
                                  , throw )
import           Control.Monad( replicateM )
import           Data.Maybe( fromMaybe )
import           Data.Time( UTCTime
                          , diffUTCTime
                          , getCurrentTime )
import           System.Random( getStdRandom
                              , randomR )
import           System.Console.ANSI( clearScreen )
import           System.IO( getChar
                          , hFlush
                          , stdout )
import           System.Timeout( timeout )


import           Console( configureConsoleFor
                        , ConsoleConfig(..)
                        , renderStrLn
                        , RenderState(..) )
import           Geo( sumCoords
                    , coordsForDirection
                    , Col(..)
                    , Coords(..)
                    , Direction(..)
                    , Row(..)
                    , zeroCoords )
import           Threading( runAndWaitForTermination
                          , Termination(..) )

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


newtype Timer = Timer { _initialTime :: UTCTime }


computeTime :: Timer -> UTCTime -> Int
computeTime (Timer t1) t2 =
  let t = diffUTCTime t2 t1
  in floor t


data PosSpeed = PosSpeed {
    _pos :: !Coords
  , _speed :: !Coords
}
data World = World{
    _ball :: ![PosSpeed]
  , _howBallMoves :: PosSpeed -> PosSpeed
}

worldSize :: Int
worldSize = 30

ballMotion :: PosSpeed -> PosSpeed
ballMotion (PosSpeed (Coords (Row r) (Col c)) (Coords (Row dr') (Col dc'))) =
    PosSpeed (Coords (Row $ r+dr) (Col $ c+dc)) (Coords (Row dr) (Col dc))
  where
    dr = if r > 0 && r < worldSize then dr' else negate dr'
    dc = if c > 0 && c < worldSize then dc' else negate dc'

data GameState = GameState {
    _startTime :: !Timer
  , _updateCounter :: !Int
  , _upperLeftCorner :: !Coords
  , _world :: !World
}

eraMicros :: Int
eraMicros = eraMillis * 1000
  where
    eraMillis = 160 -- this controls the game loop frequency.
                    -- 20 seems to match screen refresh frequency

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
showTimer currentTime (GameState startTime updateTick _ _) =
  let time = computeTime startTime currentTime
  in "|" ++ showUpdateTick updateTick ++ "| " ++ show time ++ " |"


nextUpdateCounter :: Int -> Int
nextUpdateCounter c = (c + 1) `mod` maxUpdateTick


--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

run :: IO Termination
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
  balls <- replicateM 20 createRandomBall
  return $ GameState (Timer t) 0 zeroCoords (World balls ballMotion)


randomPos :: IO Int
randomPos = getStdRandom $ randomR (0,worldSize-1)


randomSpeed :: IO Int
randomSpeed = getStdRandom $ randomR (-2,2)


createRandomBall :: IO PosSpeed
createRandomBall = do
  x <- randomPos
  y <- randomPos
  dx <- randomSpeed
  dy <- randomSpeed
  return $ PosSpeed (Coords (Row x) (Col y)) (Coords (Row dx) (Col dy))


loop :: GameState -> IO ()
loop state@(GameState _ _ coords _) = do
  let r = RenderState coords
  updateGame state r >>= loop


printTimer :: GameState -> RenderState -> IO RenderState
printTimer s r = do
  t <- getCurrentTime
  renderStrLn r $ showTimer t s


updateGame :: GameState -> RenderState -> IO GameState
updateGame s r =
  (clearScreen >> getAction >>= renderGame s r) `finally` hFlush stdout


getAction :: IO (Maybe Action)
getAction = do
  a <- timeout eraMicros getChar >>= mapM (return . actionFromChar)
  return $ fromMaybe Nothing a


renderGame :: GameState -> RenderState -> Maybe Action -> IO GameState
renderGame state@(GameState t c frameCorner (World balls fBallMotion)) (RenderState renderCorner) maybeAction = do
  -- TODO make this generic
  let frameOffset = case maybeAction of
        (Just (Frame a)) -> coordsForDirection a
        _ -> zeroCoords
      r = RenderState $ sumCoords renderCorner frameOffset
  r2 <- printTimer state r

  _ <- case maybeAction of
    (Just Throw) -> do
      _ <- renderStrLn r2 "Boom! An overflow exception was thrown in the game thread."
      throw Overflow
    _ -> return ()

  mapM_ (renderWorld r2) balls
  return $ GameState t (nextUpdateCounter c) (sumCoords frameCorner frameOffset) $ World (map fBallMotion balls) ballMotion


-- TODO returned RenderState should be at the bottom of the world
renderWorld :: RenderState -> PosSpeed -> IO RenderState
renderWorld (RenderState renderCoords) (PosSpeed worldCoords _) = renderStrLn loc "O"
  where loc = RenderState $ sumCoords renderCoords worldCoords
