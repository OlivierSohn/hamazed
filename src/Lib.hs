
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
worldSize = 30


inWorld :: Coords -> Bool
inWorld (Coords (Row r) (Col c)) = inWorldCoord r && inWorldCoord c
  where inWorldCoord x = x >= 0 && x < worldSize

ballMotion :: PosSpeed -> PosSpeed
ballMotion (PosSpeed (Coords (Row r) (Col c)) (Coords (Row dr) (Col dc))) =
    mirrorIfNeeded newPosSpeed
  where
    newR = r + dr
    newC = c + dc
    newPosSpeed = PosSpeed (Coords (Row newR) (Col newC)) (Coords (Row dr) (Col dc))


mirrorSpeedIfNeeded :: Int -> Int -> Int
mirrorSpeedIfNeeded x dx = if x > 0 && x < worldSize-1 then dx else negate dx

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
  balls <- replicateM 20 createRandomPosSpeed
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


getAction :: IO Action
getAction = do
  a <- timeout eraMicros getChar >>= mapM (return . actionFromChar)
  return $ fromMaybe Timeout a


renderGame :: GameState -> RenderState -> Action -> IO GameState
renderGame state@(GameState t c frameCorner world@(World balls _ ship)) (RenderState renderCorner) action = do
  -- adjust frame position
  let frameOffset = coordsFor Frame action

  r2 <- printTimer state $ RenderState $ sumCoords renderCorner frameOffset

  _ <- case action of
    Throw -> do
      _ <- renderStrLn r2 "Boom! An overflow exception was thrown in the game thread."
      throw Overflow
    _ -> return ()

  mapM_ (render r2 'O') balls
  _ <- render r2 '+' ship
  return $ GameState t (nextUpdateCounter c) (sumCoords frameCorner frameOffset) $ nextWorld action world


-- TODO returned RenderState should be at the bottom of the world
render :: RenderState -> Char -> PosSpeed -> IO RenderState
render r@(RenderState renderCoords) chr (PosSpeed worldCoords _) =
  if inWorld worldCoords
  then renderStrLn loc [chr]
  else return r
  where loc = RenderState $ sumCoords renderCoords worldCoords
