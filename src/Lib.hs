{-# LANGUAGE LambdaCase #-}

module Lib
    ( run
    ) where


import           Control.Concurrent( threadDelay )
import           Control.Exception( ArithException(..)
                                  , finally
                                  , throw )
import           Control.Monad.Loops( unfoldM )
import           Data.Char( intToDigit )
import           Data.Maybe( mapMaybe )
import           Data.Time( UTCTime
                          , getCurrentTime )
import           System.Console.ANSI( clearScreen )
import           System.IO( getChar
                          , hFlush
                          , hReady
                          , stdin
                          , stdout )


import           Console( configureConsoleFor
                        , ConsoleConfig(..)
                        , renderSegment
                        , renderStrLn
                        , RenderState(..) )
import           Geo( sumCoords
                    , Col(..)
                    , Coords(..)
                    , Direction(..)
                    , PosSpeed(..)
                    , Row(..)
                    , Segment(..)
                    , segmentContains
                    , translateCoord
                    , zeroCoords )
import           Laser( LaserType(..)
                      , laserChar
                      , shootLaserFromShip )
import           Threading( runAndWaitForTermination
                          , Termination(..) )
import           Timing( computeTime
                       , eraMicros
                       , nextUpdateCounter
                       , showUpdateTick
                       , Timer(..) )
import           World( Action(..)
                      , ActionTarget(..)
                      , actionFromChar
                      , coordsForActionTargets
                      , Location(..)
                      , location
                      , mkWorld
                      , nextWorld
                      , World(..)
                      , Number(..)
                      , worldSize )


--------------------------------------------------------------------------------
-- Pure
--------------------------------------------------------------------------------

data GameState = GameState {
    _startTime :: !Timer
  , _updateCounter :: !Int
  , _upperLEFTCorner :: !Coords
  , _world :: !World
}


data LaserRay = LaserRay {
    _laserRayDir :: !Direction
  , _laserRaySeg :: !Segment
}


showTimer :: UTCTime -> GameState -> String
showTimer currentTime (GameState startTime updateTick _ _) =
  let time = computeTime startTime currentTime
  in "|" ++ showUpdateTick updateTick ++ "| " ++ show time ++ " |"

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
  world <- mkWorld
  return $ GameState (Timer t) 0 zeroCoords world


loop :: GameState -> IO ()
loop state@(GameState _ _ coords _) = do
  threadDelay eraMicros
  let r = RenderState coords
  updateGame state r >>= loop


printTimer :: GameState -> RenderState -> IO RenderState
printTimer s r = do
  t <- getCurrentTime
  renderStrLn (showTimer t s) r


updateGame :: GameState -> RenderState -> IO GameState
updateGame s r =
  (clearScreen >> getActions >>= renderGame s r) `finally` hFlush stdout


readOneChar :: IO (Maybe Char)
readOneChar = do
  hasMore <- hReady stdin
  if hasMore
    then do
      c <- getChar
      return $ Just c
    else
      return Nothing


getActions :: IO [Action]
getActions = do
  inputs <- unfoldM readOneChar
  return $ map actionFromChar inputs


renderGame :: GameState -> RenderState -> [Action] -> IO GameState
renderGame state@(GameState t c frameCorner world@(World balls _ (PosSpeed shipCoords _))) (RenderState renderCorner) actions = do
  -- modify world according to actions:
  --   Frame /
  let frameOffset = coordsForActionTargets Frame actions

  -- render timer
  r <- printTimer state $ RenderState $ sumCoords renderCorner frameOffset
  -- render enclosing rectangle
  r2 <- renderWorldFrame r

  -- make lasers
  let laserRays = mapMaybe
        (\case
          (Action Laser dir) ->
            case shootLaserFromShip shipCoords dir Infinite of
              (Just ray) -> Just $ LaserRay dir ray
              Nothing -> Nothing
          Throw -> throw Overflow
          _     -> Nothing
        ) actions

  -- render lasers
  mapM_ (\(LaserRay dir seg) -> renderSegment seg (laserChar dir) r2) laserRays

  -- render numbers
  mapM_ (\(Number (PosSpeed b _) i) -> render r2 (intToDigit i) b) balls

  -- compute remaining numbers
  let newBalls = filter (\(Number (PosSpeed b _) _) -> (not (any (\(LaserRay _ seg) -> segmentContains b seg) laserRays))) balls
  -- render ship
  _ <- render r2 '+' shipCoords
  return $ GameState t (nextUpdateCounter c) (sumCoords frameCorner frameOffset) $ nextWorld actions world newBalls


renderWorldFrame :: RenderState -> IO RenderState
renderWorldFrame upperLEFT@(RenderState upperLEFTCoords) = do
  let horizontalWall = replicate (worldSize + 2)
      lowerLEFT = RenderState $ sumCoords upperLEFTCoords $ Coords (Row $ worldSize+1) (Col 0)

  -- upper wall
  (RenderState renderCoords) <- renderStrLn (horizontalWall '_') upperLEFT
  let worldCoords = translateCoord RIGHT renderCoords

  -- left & right walls
  let leftWallCoords = take worldSize $ iterate (translateCoord Down) renderCoords
      toRIGHT = Coords (Row 0) (Col $ worldSize+1)
      rightWallCoords = take worldSize $ iterate (translateCoord Down) $ sumCoords renderCoords toRIGHT
  mapM_ (renderStrLn ['|'] . RenderState) (leftWallCoords ++ rightWallCoords)

  -- lower wall
  _ <- renderStrLn (horizontalWall 'T') lowerLEFT
  return $ RenderState worldCoords


render :: RenderState -> Char -> Coords -> IO RenderState
render r@(RenderState renderCoords) char worldCoords =
  case location worldCoords of
    InsideWorld -> renderStrLn [char] loc
    _           -> return r
  where loc = RenderState $ sumCoords renderCoords worldCoords
