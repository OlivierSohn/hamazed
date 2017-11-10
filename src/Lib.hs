
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
                        , renderSegment
                        , renderStrLn
                        , RenderState(..) )
import           Geo( sumCoords
                    , coordsForDirection
                    , Col(..)
                    , Coords(..)
                    , Direction(..)
                    , Row(..)
                    , segmentContains
                    , translateCoord
                    , zeroCoords )
import           Laser( LaserType(..)
                      , laserChar
                      , shootLaserFromShip )
import           Threading( runAndWaitForTermination
                          , Termination(..) )
import           World( Action(..)
                      , ActionTarget(..)
                      , actionFromChar
                      , Location(..)
                      , location
                      , worldSize )


--------------------------------------------------------------------------------
-- Pure
--------------------------------------------------------------------------------

coordsFor :: ActionTarget -> Action -> Coords
coordsFor target action = fromMaybe zeroCoords $ maybeCoordsFor target action


maybeCoordsFor :: ActionTarget -> Action -> Maybe Coords
maybeCoordsFor targetFilter (Action actionTarget dir)
   | actionTarget == targetFilter = Just $ coordsForDirection dir
   | otherwise = Nothing
maybeCoordsFor _ _ = Nothing


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
    _worldNumber :: ![Number]
  , _howBallMoves :: PosSpeed -> PosSpeed
  , _worldShip :: !PosSpeed
}
data Number = Number {
    _numberPosSpeed :: !PosSpeed
  , _numberNum :: !Int
}

nextWorld :: Action -> World -> [Number] -> World
nextWorld action (World _ move (PosSpeed shipPos shipSpeed)) balls =
  let shipAcceleration = coordsFor Ship action
      ship = PosSpeed shipPos $ sumCoords shipSpeed shipAcceleration
  in World (map (\(Number ps n) -> Number (move ps) n) balls) move (move ship)


ballMotion :: PosSpeed -> PosSpeed
ballMotion = doBallMotion . mirrorIfNeeded

doBallMotion :: PosSpeed -> PosSpeed
doBallMotion (PosSpeed (Coords (Row r) (Col c)) (Coords (Row dr) (Col dc))) =
    PosSpeed (Coords (Row newR) (Col newC)) (Coords (Row dr) (Col dc))
  where
    newR = r + dr
    newC = c + dc


mirrorSpeedIfNeeded :: Int -> Int -> Int
mirrorSpeedIfNeeded x
  | x <= 0           = abs
  | x >= worldSize-1 = negate . abs
  | otherwise        = id

{--
constrainPos :: Int -> Int
constrainPos r
  | r < 0 = negate r
  | r >= worldSize = 2 * worldSize - r
  | otherwise = r

constrainPosSticky :: Int -> Int
constrainPosSticky r
  | r < 0 = 0
  | r >= worldSize = worldSize - 1
  | otherwise = r
--}

mirrorIfNeeded :: PosSpeed -> PosSpeed
mirrorIfNeeded (PosSpeed (Coords (Row r) (Col c)) (Coords (Row dr) (Col dc))) =
  let newDr = mirrorSpeedIfNeeded r dr
      newDc = mirrorSpeedIfNeeded c dc
      -- we chose to not constrain the positions as it leads to unnatural motions
      -- the tradeoff is just to not render them
      newR = r
      newC = c

  in PosSpeed (Coords (Row newR) (Col newC)) (Coords (Row newDr) (Col newDc))

data GameState = GameState {
    _startTime :: !Timer
  , _updateCounter :: !Int
  , _upperLEFTCorner :: !Coords
  , _world :: !World
}


eraMicros :: Int
eraMicros = eraMillis * 1000
  where
    eraMillis = 160 -- this controls the game loop frequency.
                    -- 20 seems to match screen refresh frequency

maxUpdateTick :: Int
maxUpdateTick = worldSize


tickRepresentationLength :: Int
tickRepresentationLength = quot maxUpdateTick 2


showUpdateTick :: Int -> String
showUpdateTick t =
  let nDotsBefore = max 0 (t + tickRepresentationLength - maxUpdateTick)
      nLEFTBlanks = t - nDotsBefore
      nDotsAfter = tickRepresentationLength - nDotsBefore
      nRIGHTBlanks = maxUpdateTick - t - tickRepresentationLength
  in replicate nDotsBefore  '.'
  ++ replicate nLEFTBlanks  ' '
  ++ replicate nDotsAfter   '.'
  ++ replicate nRIGHTBlanks ' '


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
  balls <- mapM createRandomNumber [0..9]
  ship <- createRandomPosSpeed
  return $ GameState (Timer t) 0 zeroCoords (World balls ballMotion ship)

createRandomNumber :: Int -> IO Number
createRandomNumber i = do
  ps <- createRandomPosSpeed
  return $ Number ps i

randomPos :: IO Int
randomPos = getStdRandom $ randomR (0,worldSize-1)


randomSpeed :: IO Int
randomSpeed = getStdRandom $ randomR (-1,1)


createRandomPosSpeed :: IO PosSpeed
createRandomPosSpeed = do
  x <- randomPos
  y <- randomPos
  dx <- randomSpeed
  dy <- randomSpeed
  return $ mirrorIfNeeded $ PosSpeed (Coords (Row x) (Col y)) (Coords (Row dx) (Col dy))


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
  let frameOffset = coordsFor Frame action

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
