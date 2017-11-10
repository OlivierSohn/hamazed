
module Lib
    ( run
    ) where


import           Control.Exception( ArithException(..)
                                  , finally
                                  , throw )
import           Data.Char( intToDigit )
import           Data.Maybe( fromMaybe
                           , mapMaybe )
import           Data.Time( UTCTime
                          , getCurrentTime )
import           System.Console.ANSI( clearScreen )
import           System.IO( getChar
                          , hFlush
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
                      , coordsForActionTarget
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
  let r = RenderState coords
  updateGame state r >>= loop


printTimer :: GameState -> RenderState -> IO RenderState
printTimer s r = do
  t <- getCurrentTime
  renderStrLn (showTimer t s) r


updateGame :: GameState -> RenderState -> IO GameState
updateGame s r =
  (clearScreen >> getAction >>= renderGame s r) `finally` hFlush stdout


getAction :: IO Action
getAction = do
  a <- timeout eraMicros getChar >>= mapM (return . actionFromChar)
  return $ fromMaybe Timeout a


renderGame :: GameState -> RenderState -> Action -> IO GameState
renderGame state@(GameState t c frameCorner world@(World balls _ (PosSpeed shipCoords _))) (RenderState renderCorner) action = do
  let frameOffset = coordsForActionTarget Frame action

  -- render timer
  r <- printTimer state $ RenderState $ sumCoords renderCorner frameOffset
  -- render enclosing rectangle
  r2 <- renderWorldFrame r

  -- make laser
  mayLaserRay <- case action of
    Action Laser dir -> do
      let res = shootLaserFromShip shipCoords dir Infinite
      -- render laser
      _ <- case res of
        (Just laserRay) -> renderSegment laserRay (laserChar dir) r2
        Nothing -> return r2
      return res
    Throw            -> throw Overflow
    _                -> return Nothing

  -- render numbers
  mapM_ (\(Number (PosSpeed b _) i) -> render r2 (intToDigit i) b) balls

  -- compute remaining numbers
  let newBalls = case mayLaserRay of
        Nothing       -> balls
        Just laserRay -> mapMaybe (\ball@(Number (PosSpeed b _) _) ->
          if segmentContains laserRay b
            then Nothing
            else Just ball) balls
  -- render ship
  _ <- render r2 '+' shipCoords
  return $ GameState t (nextUpdateCounter c) (sumCoords frameCorner frameOffset) $ nextWorld action world newBalls


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
