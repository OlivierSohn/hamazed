
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
                        , renderLine
                        , renderStrLn
                        , RenderState(..) )
import           Geo( sumCoords
                    , coordsForDirection
                    , Col(..)
                    , Coords(..)
                    , Direction(..)
                    , Row(..)
                    , translateCoord
                    , zeroCoords )
import           Threading( runAndWaitForTermination
                          , Termination(..) )

--------------------------------------------------------------------------------
-- Pure
--------------------------------------------------------------------------------

data Action = Action ActionTarget Direction |
              Timeout |
              Nonsense |
              Throw deriving (Show)

data ActionTarget = Frame | Ship | Laser deriving(Eq, Show)


coordsFor :: ActionTarget -> Action -> Coords
coordsFor target action = fromMaybe zeroCoords $ maybeCoordsFor target action


maybeCoordsFor :: ActionTarget -> Action -> Maybe Coords
maybeCoordsFor targetFilter (Action actionTarget dir)
   | actionTarget == targetFilter = Just $ coordsForDirection dir
   | otherwise = Nothing
maybeCoordsFor _ _ = Nothing

actionFromChar :: Char -> Action
actionFromChar c = case c of
  'o' -> Throw
  'g' -> Action Frame Down
  't' -> Action Frame Up
  'f' -> Action Frame Left
  'h' -> Action Frame Right
  'k' -> Action Laser Down
  'i' -> Action Laser Up
  'j' -> Action Laser Left
  'l' -> Action Laser Right
  's' -> Action Ship Down
  'w' -> Action Ship Up
  'a' -> Action Ship Left
  'd' -> Action Ship Right
  _   -> Nonsense


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
  , _ship :: !PosSpeed
}


nextWorld :: Action -> World -> World
nextWorld action (World balls move (PosSpeed shipPos shipSpeed)) =
  let shipAcceleration = coordsFor Ship action
      ship = PosSpeed shipPos $ sumCoords shipSpeed shipAcceleration
  in World (map move balls) move (move ship)


worldSize :: Int
worldSize = 35

nBalls :: Int
nBalls = 20

data Location = InsideWorld | OutsideWorld

location :: Coords -> Location
location (Coords (Row r) (Col c))
  | inside r && inside c = InsideWorld
  | otherwise            = OutsideWorld
  where inside x = x >= 0 && x < worldSize

ballMotion :: PosSpeed -> PosSpeed
ballMotion (PosSpeed (Coords (Row r) (Col c)) (Coords (Row dr) (Col dc))) =
    mirrorIfNeeded newPosSpeed
  where
    newR = r + dr
    newC = c + dc
    newPosSpeed = PosSpeed (Coords (Row newR) (Col newC)) (Coords (Row dr) (Col dc))


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
  , _upperLeftCorner :: !Coords
  , _world :: !World
}


laserChar :: Direction -> Char
laserChar dir = case dir of
  Up -> '|'
  Down -> '|'
  Left -> '-'
  Right -> '-'


extend :: Coords -> Direction -> Coords
extend (Coords (Row r) (Col c)) dir = case dir of
  Up -> Coords (Row 0) (Col c)
  Left -> Coords (Row r) (Col 0)
  Down -> Coords (Row (worldSize-1)) (Col c)
  Right -> Coords (Row r) (Col (worldSize-1))


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
  balls <- replicateM nBalls createRandomPosSpeed
  ship <- createRandomPosSpeed
  return $ GameState (Timer t) 0 zeroCoords (World balls ballMotion ship)


randomPos :: IO Int
randomPos = getStdRandom $ randomR (0,worldSize-1)


randomSpeed :: IO Int
randomSpeed = getStdRandom $ randomR (-2,2)


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
renderGame state@(GameState t c frameCorner world@(World balls _ ship@(PosSpeed shipCoords _))) (RenderState renderCorner) action = do
  let frameOffset = coordsFor Frame action

  -- render timer
  r <- printTimer state $ RenderState $ sumCoords renderCorner frameOffset
  -- render enclosing rectangle
  r2 <- renderWorldFrame r

  -- render balls
  mapM_ (render r2 'O') balls
  -- render laser
  _ <- case action of
    Action Laser dir -> renderLaser shipCoords dir r2
    Throw            -> throw Overflow
    _                -> return r2
  -- render ship
  _ <- render r2 '+' ship
  return $ GameState t (nextUpdateCounter c) (sumCoords frameCorner frameOffset) $ nextWorld action world


renderLaser :: Coords -> Direction -> RenderState -> IO RenderState
renderLaser c1 dir = do
  let c2 = extend c1 dir
  renderLine c1 c2 $ laserChar dir


renderWorldFrame :: RenderState -> IO RenderState
renderWorldFrame upperLeft@(RenderState upperLeftCoords) = do
  let horizontalWall = replicate (worldSize + 2)
      lowerLeft = RenderState $ sumCoords upperLeftCoords $ Coords (Row $ worldSize+1) (Col 0)

  -- upper wall
  (RenderState renderCoords) <- renderStrLn (horizontalWall '_') upperLeft
  let worldCoords = translateCoord Right renderCoords

  -- left & right walls
  let leftWallCoords = take worldSize $ iterate (translateCoord Down) renderCoords
      toRight = Coords (Row 0) (Col $ worldSize+1)
      rightWallCoords = take worldSize $ iterate (translateCoord Down) $ sumCoords renderCoords toRight
  mapM_ (renderStrLn ['|'] . RenderState) (leftWallCoords ++ rightWallCoords)

  -- lower wall
  _ <- renderStrLn (horizontalWall 'T') lowerLeft
  return $ RenderState worldCoords


render :: RenderState -> Char -> PosSpeed -> IO RenderState
render r@(RenderState renderCoords) char (PosSpeed worldCoords _) =
  case location worldCoords of
    InsideWorld -> renderStrLn [char] loc
    _           -> return r
  where loc = RenderState $ sumCoords renderCoords worldCoords
