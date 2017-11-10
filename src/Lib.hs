{-# LANGUAGE LambdaCase #-}

module Lib
    ( run
    ) where


import           Control.Exception( ArithException(..)
                                  , finally
                                  , throw )
import           Data.Char( intToDigit )
import           Data.Maybe( mapMaybe )
import           Data.Time( UTCTime
                          , diffUTCTime
                          , getCurrentTime )
import           System.Console.ANSI( clearScreen )
import           System.IO( getChar
                          , hFlush
                          , hReady
                          , stdin
                          , stdout )
import           System.Timeout( timeout )

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
import           Timing( addMotionStepDuration
                       , computeTime
                       , nextUpdateCounter
                       , showUpdateTick
                       , Timer(..) )
import           World( Action(..)
                      , ActionTarget(..)
                      , actionFromChar
                      , coordsForActionTargets
                      , Location(..)
                      , location
                      , moveWorld
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
  , _nextMotionStep :: !UTCTime
  , _updateCounter :: !Int
  , _upperLeftCorner :: !Coords
  , _world :: !World
}

data LaserRay = LaserRay {
    _laserRayDir :: !Direction
  , _laserRaySeg :: !Segment
}


showTimer :: UTCTime -> GameState -> String
showTimer currentTime (GameState startTime _ updateTick _ _) =
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
  return $ GameState (Timer t) t 0 zeroCoords world


loop :: GameState -> IO ()
loop state@(GameState _ _ _ coords _) = do
  let r = RenderState coords
  updateGame state r >>= loop


printTimer :: GameState -> RenderState -> IO RenderState
printTimer s r = do
  t <- getCurrentTime
  renderStrLn (showTimer t s) r


updateGame :: GameState -> RenderState -> IO GameState
updateGame state@(GameState a _ c d world) r = do
  action <- getAction state
  case action of
    Nonsense -> return state
    Timeout  -> do
      -- change position of objects in the world and compute the new deadline
      curTime <- getCurrentTime
      let newState = GameState a (addMotionStepDuration curTime) c d (moveWorld world)
      updateGame2 action newState r
    _        ->
      updateGame2 action state r


updateGame2 :: Action -> GameState -> RenderState -> IO GameState
updateGame2 a s r = do
  clearScreen
  res <- renderGame s r a
  hFlush stdout
  return res


readOneChar :: IO (Maybe Char)
readOneChar = do
  hasMore <- hReady stdin
  if hasMore
    then do
      c <- getChar
      return $ Just c
    else
      return Nothing

{--
import           Control.Monad.Loops( unfoldM )

getActions :: IO [Action]
getActions = do
  inputs <- unfoldM readOneChar
  return $ map actionFromChar inputs
--}


getAction :: GameState -> IO Action
getAction (GameState _ nextMotionStep _ _ _) = do
  t <- getCurrentTime
  let remainingSeconds = diffUTCTime nextMotionStep t
      remainingMicros = floor (remainingSeconds * 10^(6 :: Int))
  if remainingMicros < 0
    then return Timeout
    else do
      maybeC <- timeout remainingMicros readOneChar
      case maybeC of
        (Just (Just c)) -> return $ actionFromChar c
        (Just _) -> return Nonsense
        _ -> return Timeout


renderGame :: GameState -> RenderState -> Action -> IO GameState
renderGame
 state@(GameState t motionStepDeadline c frameCorner world@(World balls _ (PosSpeed shipCoords _)))
 (RenderState renderCorner)
 action = do
  let actions = [action]
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

  -- render numbers, including the ones that will be destroyed, if any
  mapM_ (\(Number (PosSpeed b _) i) -> render r2 (intToDigit i) b) balls

  -- render ship
  _ <- render r2 '+' shipCoords
  -- compute remaining numbers
  let newBalls = filter (\(Number (PosSpeed b _) _) -> (not (any (\(LaserRay _ seg) -> segmentContains b seg) laserRays))) balls
  return $ GameState t motionStepDeadline (nextUpdateCounter c) (sumCoords frameCorner frameOffset) $ nextWorld action world newBalls


renderWorldFrame :: RenderState -> IO RenderState
renderWorldFrame upperLeft@(RenderState upperLeftCoords) = do
  let horizontalWall = replicate (worldSize + 2)
      lowerLEFT = RenderState $ sumCoords upperLeftCoords $ Coords (Row $ worldSize+1) (Col 0)

  -- upper wall
  (RenderState renderCoords) <- renderStrLn (horizontalWall '_') upperLeft
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
