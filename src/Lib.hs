{-# LANGUAGE LambdaCase #-}

module Lib
    ( run
    ) where


import           Control.Exception( ArithException(..)
                                  , finally
                                  , throw )
import           Control.Monad( when )
import           Data.Char( intToDigit )
import           Data.Maybe( maybe )
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
                        , renderChar_
                        , renderSegment
                        , renderStrLn
                        , RenderState(..) )
import           Geo( sumCoords
                    , Col(..)
                    , Coords(..)
                    , coordsForDirection
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
  , _gameStateLaserRay :: !(Maybe LaserRay)
}

data LaserRay = LaserRay {
    _laserRayDir :: !Direction
  , _laserRaySeg :: !Segment
}


survivingNumbers :: [Number] -> LaserRay -> [Number]
survivingNumbers l (LaserRay _ seg) = filter (\(Number (PosSpeed pos _) _) -> (not $ segmentContains pos seg)) l

nextGameState :: GameState -> Action -> GameState
nextGameState (GameState a b c d world@(World balls _ (PosSpeed shipCoords _)) _) action =
  let maybeLaserRay = case action of
        (Action Laser dir) -> LaserRay dir <$> shootLaserFromShip shipCoords dir Infinite
        _     -> Nothing
      remainingBalls = maybe balls (survivingNumbers balls) maybeLaserRay
  in GameState a b c d (nextWorld action world remainingBalls) maybeLaserRay

shipCollides :: World -> Bool
shipCollides (World balls _ (PosSpeed shipCoords _)) =
   any (\(Number (PosSpeed pos _) _) -> shipCoords == pos) balls

showTimer :: UTCTime -> GameState -> String
showTimer currentTime (GameState startTime _ updateTick _ _ _) =
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
  return $ GameState (Timer t) t 0 zeroCoords world Nothing


loop :: GameState -> IO ()
loop state@(GameState _ _ _ coords _ _) = do
  let r = RenderState coords
  updateGame state r >>= loop


printTimer :: GameState -> RenderState -> IO RenderState
printTimer s r = do
  t <- getCurrentTime
  renderStrLn (showTimer t s) r


updateGame :: GameState -> RenderState -> IO GameState
updateGame state@(GameState a b c d world f) r =
  getAction state >>=
    (\action -> case action of
      Nonsense -> return state
      Throw    -> throw Overflow
      _        -> updateGame2 action r =<<
        (case action of
          Timeout  -> do
            -- change position of objects in the world and compute the new deadline
            curTime <- getCurrentTime
            let newWorld = moveWorld world
                newState = GameState a (addMotionStepDuration curTime) (nextUpdateCounter c) d newWorld f
            when (shipCollides newWorld) $ throw Overflow
            return newState
          (Action Frame dir) -> do
            -- offset the drawing frame
            let frameOffset = coordsForDirection dir
            return $ GameState a b c (sumCoords d frameOffset) world f
          _        -> return state
        ))

updateGame2 :: Action -> RenderState -> GameState -> IO GameState
updateGame2 a r s = do
  clearScreen
  let res = nextGameState s a
  renderGame res r
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
getAction (GameState _ nextMotionStep _ _ _ _) = do
  t <- getCurrentTime
  let remainingSeconds = diffUTCTime nextMotionStep t
      remainingMicros = floor (remainingSeconds * 10^(6 :: Int))
  if remainingMicros < 0
    then return Timeout
    else
      (\case
        (Just (Just c)) -> actionFromChar c
        (Just _) -> Nonsense
        _ -> Timeout
        ) <$> timeout remainingMicros readOneChar


renderGame :: GameState -> RenderState -> IO ()
renderGame state@(GameState _ _ _ _ (World balls _ (PosSpeed shipCoords _)) maybeLaserRay) frameCorner = do
  _ <- printTimer state frameCorner >>= renderWorldFrame >>= (\worldCorner -> do
    _ <- case maybeLaserRay of
      (Just (LaserRay laserDir laserSeg)) -> renderSegment laserSeg (laserChar laserDir) worldCorner
      Nothing -> return worldCorner
    -- render numbers, including the ones that will be destroyed, if any
    mapM_ (\(Number (PosSpeed pos _) i) -> render_ (intToDigit i) pos worldCorner) balls
    render_ '+' shipCoords worldCorner)
  return ()


renderWorldFrame :: RenderState -> IO RenderState
renderWorldFrame upperLeft@(RenderState upperLeftCoords) = do
  let horizontalWall = replicate (worldSize + 2)
      lowerLeft = RenderState $ sumCoords upperLeftCoords $ Coords (Row $ worldSize+1) (Col 0)

  -- upper wall
  (RenderState renderCoords) <- renderStrLn (horizontalWall '_') upperLeft
  let worldCoords = translateCoord RIGHT renderCoords

  -- left & right walls
  let leftWallCoords = take worldSize $ iterate (translateCoord Down) renderCoords
      toRight = Coords (Row 0) (Col $ worldSize+1)
      rightWallCoords = take worldSize $ iterate (translateCoord Down) $ sumCoords renderCoords toRight
  mapM_ (renderChar_ '|' . RenderState) (leftWallCoords ++ rightWallCoords)

  -- lower wall
  _ <- renderStrLn (horizontalWall 'T') lowerLeft
  return $ RenderState worldCoords


render_ :: Char -> Coords -> RenderState -> IO ()
render_ char worldCoords (RenderState renderCoords) =
  case location worldCoords of
    InsideWorld -> renderChar_ char loc
    _           -> return ()
  where loc = RenderState $ sumCoords renderCoords worldCoords
