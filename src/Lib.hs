{-# LANGUAGE LambdaCase #-}

module Lib
    ( run
    ) where


import           Control.Applicative( (<|>) )
import           Control.Exception( finally )
import           Control.Monad( when )

import           Data.Char( intToDigit )
import           Data.List( minimumBy
                          , partition )
import           Data.Maybe( mapMaybe
                           , isJust
                           , isNothing
                           , maybe )
import           Data.Time( UTCTime
                          , addUTCTime
                          , diffUTCTime
                          , getCurrentTime )

import           System.Console.ANSI( clearScreen
                                    , setSGR
                                    , SGR(..)
                                    , ConsoleLayer(..)
                                    , ColorIntensity(..)
                                    , Color(..) )
import qualified System.Console.Terminal.Size as Terminal(size
                                                         , Window(..))
import           System.IO( getChar
                          , hFlush
                          , stdout )
import           System.Timeout( timeout )

import           Animation( quantitativeExplosionThenSimpleExplosion
                          , renderAnimations
                          , simpleExplosion )
import           Console( configureConsoleFor
                        , ConsoleConfig(..)
                        , renderChar_
                        , renderSegment
                        , renderStrLn
                        , renderStrLn_
                        , RenderState(..) )
import           Geo( sumCoords
                    , Col(..)
                    , Coords(..)
                    , coordsForDirection
                    , Direction(..)
                    , PosSpeed(..)
                    , Row(..)
                    , segmentContains
                    , translate
                    , translateInDir
                    , translateLeft )
import           Laser( LaserType(..)
                      , laserChar
                      , Ray(..)
                      , Theoretical
                      , Actual
                      , shootLaserFromShip
                      , stopRayAtFirstCollision )
import           Threading( runAndWaitForTermination
                          , Termination(..) )
import           Timing( addMotionStepDuration
                       , nextUpdateCounter
                       --, computeTime
                       --, showUpdateTick
                       , Timer(..) )
import           Util( showListOrSingleton )
import           World( Action(..)
                      , ActionTarget(..)
                      , Animation(..)
                      , BattleShip(..)
                      , earliestAnimationDeadline
                      , Location(..)
                      , location
                      , moveWorld
                      , mkAnimation
                      , mkWorld
                      , nextWorld
                      , Step(..)
                      , stepEarliestAnimations
                      , World(..)
                      , Number(..) )
import           WorldSize( WorldSize(..) )


--------------------------------------------------------------------------------
-- Pure
--------------------------------------------------------------------------------

data GameState = GameState {
    _startTime :: !Timer
  , _curTime :: !UTCTime
  , _nextMotionStep :: !UTCTime
  , _updateCounter :: !Int
  , _upperLeftCorner :: !Coords
  , _world :: !World
  , _gameStateLaserRay :: !(Maybe (LaserRay Actual))
  , _gameShotNumbers :: ![Int]
  , _gameTarget :: !Int
  , _gameStateLevel :: !Level
}

data Level = Level {
    _levelNumber :: !Int
  , _levelStatus :: !(Maybe LevelFinished)
}

lastLevel :: Int
lastLevel = 12

firstLevel :: Int
firstLevel = 1

data LevelFinished = LevelFinished {
    _levelFinishedResult :: !GameStops
  , _levelFinishedWhen :: !UTCTime
  , _levelFinishedCurrentMessage :: !MessageState
}

data MessageState = InfoMessage
                  | ContinueMessage
                  deriving(Eq, Show)

data GameStops = Lost String
               | Won

data LaserRay a = LaserRay {
    _laserRayDir :: !Direction
  , _laserRaySeg :: !(Ray a)
}

data LaserPolicy = RayDestroysFirst | RayDestroysAll

actionFromChar :: Level -> Char -> Action
actionFromChar (Level n finished) char = case finished of
  Nothing -> actionFromCharInGame char
  Just (LevelFinished stop _ ContinueMessage) ->
    case stop of
      Won      -> if n < lastLevel then StartLevel (succ n) else EndGame
      (Lost _) -> StartLevel firstLevel
  _ -> Nonsense -- between level end and proposal to continue

actionFromCharInGame :: Char -> Action
actionFromCharInGame c = case c of
  -- disabling possibility to move frame, since it is placed in the center of the terminal automatically
  {--
  'g' -> Action Frame Down
  't' -> Action Frame Up
  'f' -> Action Frame LEFT
  'h' -> Action Frame RIGHT
--}
  'k' -> Action Laser Down
  'i' -> Action Laser Up
  'j' -> Action Laser LEFT
  'l' -> Action Laser RIGHT
  's' -> Action Ship Down
  'w' -> Action Ship Up
  'a' -> Action Ship LEFT
  'd' -> Action Ship RIGHT
  _   -> Nonsense


nextGameState :: GameState -> Action -> GameState
nextGameState (GameState a t b c d world@(World balls _ (BattleShip (PosSpeed shipCoords _) ammo safeTime collisions) sz animations) _ g target (Level i finished)) action =
  let (maybeLaserRayTheoretical, newAmmo) = if ammo > 0 then case action of
          (Action Laser dir) -> (LaserRay dir <$> shootLaserFromShip shipCoords dir Infinite sz, pred ammo)
          _     -> (Nothing, ammo)
        else (Nothing, ammo)
      ((remainingBalls, destroyedBalls), maybeLaserRay) = maybe ((balls,[]), Nothing) (survivingNumbers balls RayDestroysFirst) maybeLaserRayTheoretical
      destroyedNumbers = map (\(Number _ n) -> n) destroyedBalls
      allShotNumbers = g ++ destroyedNumbers
      newAnimations = (case destroyedBalls of
        Number (PosSpeed pos _) n:_ -> mkAnimation (quantitativeExplosionThenSimpleExplosion [] n pos) t : animations
        _ -> animations)
        ++ case action of
            Timeout GameStep -> [mkAnimation (simpleExplosion shipCoords) t | not (null collisions) && isNothing safeTime]
            _ -> []
      newWorld = nextWorld action world remainingBalls newAmmo newAnimations
      newFinished = finished <|> computeFinished t newWorld (sum allShotNumbers) target action
      newLevel = Level i newFinished
  in  GameState a t b c d newWorld maybeLaserRay allShotNumbers target newLevel

computeFinished :: UTCTime -> World -> Int -> Int -> Action -> Maybe LevelFinished
computeFinished time (World _ _ (BattleShip _ ammo safeTime collisions) _ _) sumNumbers target lastAction =
    maybe Nothing (\stop -> Just $ LevelFinished stop time InfoMessage) allChecks
  where
    allChecks = checkShipCollision <|> checkSum <|> checkAmmo

    checkShipCollision = case lastAction of
      (Timeout GameStep) ->
        if isJust safeTime then Nothing
          else case map (\(Number _ n) -> n) collisions of
            [] -> Nothing
            l  -> Just $ Lost $ "collision with " ++ showListOrSingleton l
      _ -> Nothing -- this optimization is to not re-do the check when nothing has moved

    checkSum = case compare sumNumbers target of
      LT -> Nothing
      EQ -> Just Won
      GT -> Just $ Lost $ "sum " ++ show sumNumbers ++ " is bigger than target " ++ show target
    checkAmmo
      | ammo <= 0 = Just $ Lost "no ammo left"
      | otherwise = Nothing

survivingNumbers :: [Number] -> LaserPolicy -> LaserRay Theoretical -> (([Number],[Number]), Maybe (LaserRay Actual))
survivingNumbers l policy (LaserRay dir theoreticalRay@(Ray seg)) = case policy of
  RayDestroysAll   -> (partition (\(Number (PosSpeed pos _) _) -> (isNothing $ segmentContains pos seg)) l, justFull)
  RayDestroysFirst ->
    let (rayActual, mayCoord) = stopRayAtFirstCollision (map (\(Number (PosSpeed pos _) _) -> pos) l) theoreticalRay
        remainingNumbers = case mayCoord of
          Nothing -> (l,[])
          (Just pos') -> partition (\(Number (PosSpeed pos _) _) -> pos /= pos') l
    in (remainingNumbers, Just $ LaserRay dir rayActual)
 where
   justFull = Just $ LaserRay dir $ Ray seg


{--
showTimer :: UTCTime -> GameState -> String
showTimer currentTime (GameState startTime _ _ updateTick _ (World _ _ _ worldSize _) _ _ _ _) =
  let time = computeTime startTime currentTime
  in "|" ++ showUpdateTick updateTick worldSize ++ "| " ++ show time ++ " |"
  -- to debug animations, add the following line to the preceeding line
  -- ++ show (map (\(Animation _ t _) -> t) anims)
--}

replaceAnimations :: [Animation] -> GameState -> GameState
replaceAnimations anims (GameState a b c d e (World wa wb wc wd _) f g h i) =
  GameState a b c d e (World wa wb wc wd anims) f g h i

data Deadline = Deadline {
    _deadlineTime :: !UTCTime
  , _deadlineType :: !Step
} deriving(Eq, Show)

earliestDeadline :: GameState -> Maybe Deadline
earliestDeadline s =
  let l = listDeadlines s
  in  if null l then Nothing else Just $ minimumBy (\(Deadline t1 _) (Deadline t2 _) -> compare t1 t2 ) l

listDeadlines :: GameState -> [Deadline]
listDeadlines state = mapMaybe (\f -> f state) [gameDeadline, animationDeadline, messageDeadline]

gameDeadline :: GameState -> Maybe Deadline
gameDeadline (GameState _ _ nextGameStep _ _ _ _ _ _ (Level _ levelFinished)) =
  maybe (Just $ Deadline nextGameStep GameStep) (const Nothing) levelFinished

animationDeadline :: GameState -> Maybe Deadline
animationDeadline (GameState _ _ _ _ _ world _ _ _ _) =
  maybe Nothing (\ti -> Just $ Deadline ti AnimationStep) $ earliestAnimationDeadline world

messageDeadline :: GameState -> Maybe Deadline
messageDeadline (GameState _ t _ _ _ _ _ _ _ (Level _ mayLevelFinished)) =
  maybe Nothing
  (\(LevelFinished _ timeFinished messageType) ->
      case messageType of
        InfoMessage ->
          let finishedSinceSeconds = diffUTCTime t timeFinished
              delay = 2
              nextMessageStep = addUTCTime (delay - finishedSinceSeconds) t
          in  Just $ Deadline nextMessageStep MessageStep
        ContinueMessage -> Nothing)
    mayLevelFinished

goDown :: RenderState -> RenderState
goDown (RenderState r) = RenderState $ translateInDir Down r

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
gameWorker = makeInitialState firstLevel >>= loop


makeInitialState :: Int -> IO GameState
makeInitialState level = do
  t <- getCurrentTime
  termSize <- Terminal.size
  let nums = [1..(3+level)] -- more and more numbers as level increases
      worldSz = 35 + 2 * (1-level) -- less and less space as level increases
      sz = worldSz + 2 -- with walls
      coords = maybe (Coords (Row 3) (Col 3)) (\(Terminal.Window h w) -> Coords (Row $ quot (h-sz) 2) (Col $ quot (w-sz) 2)) termSize
  world <- mkWorld (WorldSize worldSz) nums
  return $ GameState (Timer t) t t 0 coords world Nothing [] (sum nums `quot` 2) $ Level level Nothing


loop :: GameState -> IO ()
loop state =
  updateGame state >>= loop


{--
printTimer :: GameState -> IO RenderState
printTimer s@(GameState _ _ _ _ r _ _ _ _ _) = do
  t <- getCurrentTime
  renderStrLn (showTimer t s) (RenderState r)
--}

updateGame :: GameState -> IO GameState
updateGame state = getAction state >>= updateGameUsingAction state

updateGameUsingAction :: GameState -> Action -> IO GameState
updateGameUsingAction state@(GameState a _ b c d world@(World _ _ _ sz _) f g h i@(Level level mayLevelFinished)) action =
  case action of
    Nonsense -> return state
    StartLevel nextLevel -> makeInitialState nextLevel
    _        -> do
      t <- getCurrentTime
      let newState = case action of
            (Timeout GameStep) -> GameState a t (addMotionStepDuration t) (nextUpdateCounter sz c) d (moveWorld t world) f g h i
            (Action Frame dir)  -> GameState a t b c (sumCoords d $ coordsForDirection dir) world f g h i
            (Timeout AnimationStep) -> GameState a t b c d (stepEarliestAnimations world) f g h i
            (Timeout MessageStep) -> -- TODO this part is ugly, we should not have to deduce so much
                                     -- MessageStep is probably the wrong abstraction level
              case mayLevelFinished of
                Just (LevelFinished stop finishTime _) -> GameState a t b c d world f g h $ Level level (Just $ LevelFinished stop finishTime ContinueMessage)
                Nothing -> state
            _ -> state
      updateGame2 action newState


updateGame2 :: Action -> GameState -> IO GameState
updateGame2 a s = do
  clearScreen
  let s2 = nextGameState s a
  animations <- renderGame s2
  let s3 = replaceAnimations animations s2
  hFlush stdout
  return s3


getAction :: GameState -> IO Action
getAction state@(GameState _ _ _ _ _ _ _ _ _ level) =
  case earliestDeadline state of
    (Just (Deadline deadline deadlineType)) -> do
      t <- getCurrentTime
      let
        timeToDeadlineSeconds = diffUTCTime deadline t
        timeToDeadlineMicros = floor (timeToDeadlineSeconds * 10^(6 :: Int))
      getActionWithinDurationMicros level timeToDeadlineMicros deadlineType
    Nothing -> actionFromChar level <$> getChar

getActionWithinDurationMicros :: Level -> Int -> Step -> IO Action
getActionWithinDurationMicros level durationMicros step =
  (\case
    Nothing   -> Timeout step
    Just char -> actionFromChar level char
    ) <$> getCharWithinDurationMicros durationMicros

getCharWithinDurationMicros :: Int -> IO (Maybe Char)
getCharWithinDurationMicros durationMicros =
  if durationMicros < 0
    then return Nothing
    else timeout durationMicros getChar

renderGame :: GameState -> IO [Animation]
renderGame state@(GameState _ _ _ _ upperLeft
                   (World _ _ (BattleShip _ ammo _ _) sz@(WorldSize worldSize) animations)
                   _ shotNumbers target (Level level _)) = do
  --printTimer state
  let r = RenderState upperLeft
      full = worldSize + 2 -- with walls
      half = quot full 2
      centerUp   = translate (Coords (Row $ -1)       (Col half)) upperLeft
      centerDown = translate (Coords (Row $ full + 1) (Col half)) upperLeft
      leftMiddle = translate (Coords (Row half)       (Col $ -1)) upperLeft
      --rightMiddle= translate (Coords (Row half)       (Col $ full + 1)) upperLeft
  _ <- renderCentered ("Level " ++ show level ++ " of " ++ show lastLevel) $ RenderState centerDown
  _ <- goDown <$> renderRightAligned ("[" ++ replicate ammo '.' ++ "]") (RenderState leftMiddle)
       >>= renderRightAligned (showShotNumbers shotNumbers)
  _ <- renderCentered ("Objective : " ++ show target) (RenderState centerUp)
  renderWorldFrame sz r >>=
    (\worldCorner -> do
      activeAnimations <- renderAnimations sz worldCorner animations
      renderWorld state worldCorner
      return activeAnimations)

renderWorld :: GameState -> RenderState -> IO ()
renderWorld (GameState _ _ _ _ _
                   (World balls _ (BattleShip (PosSpeed shipCoords _) _ safeTime collisions) sz@(WorldSize s) _)
                   maybeLaserRay _ _ (Level level levelState)) worldCorner@(RenderState upperLeft) = do
  _ <- case maybeLaserRay of
    (Just (LaserRay laserDir (Ray laserSeg))) -> renderSegment laserSeg (laserChar laserDir) worldCorner
    Nothing -> return worldCorner
  -- render numbers, including the ones that will be destroyed, if any
  mapM_ (\(Number (PosSpeed pos _) i) -> render_ (intToDigit i) pos sz worldCorner) balls
  when (null collisions) (do
    let shipColor = if isNothing safeTime then Blue else Red
    setSGR [SetColor Foreground Vivid shipColor]
    render_ '+' shipCoords sz worldCorner
    setSGR [SetColor Foreground Vivid White])
  let
    full = s
    half = quot full 2
    rightMiddle = translate (Coords (Row half) (Col $ full + 2)) upperLeft
  mapM_ (renderLevelState (RenderState rightMiddle) level) levelState

renderLevelState :: RenderState -> Int -> LevelFinished -> IO ()
renderLevelState (RenderState coords) level (LevelFinished stop _ messageState) = do
  let color = case stop of
        (Lost _) -> Yellow
        Won      -> Green
      topLeft = RenderState $ translateInDir RIGHT coords
  setSGR [SetColor Foreground Vivid color]
  afterFirst <- renderStrLn (case stop of
    (Lost reason) -> "You Lose (" ++ reason ++ ")"
    Won           -> "You Win!") topLeft
  setSGR [SetColor Foreground Vivid White]
  when (messageState == ContinueMessage) $ do
    let from = goDown afterFirst
    _ <- renderStrLn (if level == lastLevel
      then "You reached the end of the game! Hit Ctrl + C to quit."
      else
        let action = case stop of
                          (Lost _) -> "restart"
                          Won      -> "continue"
        in "Hit any key to " ++ action ++ " ...") from
    return ()

showShotNumbers :: [Int] -> String
showShotNumbers nums =
  "[" ++ unwords (map show nums) ++ "]"

renderCentered :: String -> RenderState -> IO RenderState
renderCentered str (RenderState centerCoords) = do
  let leftCorner = RenderState $ translateLeft (quot (length str) 2) centerCoords
  renderStrLn_ str leftCorner
  return $ RenderState $ translateInDir Down centerCoords

renderRightAligned :: String -> RenderState -> IO RenderState
renderRightAligned str (RenderState rightAlignment) = do
  let leftCorner = RenderState $ translateLeft (length str) rightAlignment
  renderStrLn_ str leftCorner
  return $ RenderState $ translateInDir Down rightAlignment

renderWorldFrame :: WorldSize -> RenderState -> IO RenderState
renderWorldFrame (WorldSize worldSize) upperLeft@(RenderState upperLeftCoords) = do
  let horizontalWall = replicate (worldSize + 2)
      lowerLeft = RenderState $ sumCoords upperLeftCoords $ Coords (Row $ worldSize+1) (Col 0)

  -- upper wall
  (RenderState renderCoords) <- renderStrLn (horizontalWall '_') upperLeft
  let worldCoords = translateInDir RIGHT renderCoords

  -- left & right walls
  let leftWallCoords = take worldSize $ iterate (translateInDir Down) renderCoords
      toRight = Coords (Row 0) (Col $ worldSize+1)
      rightWallCoords = take worldSize $ iterate (translateInDir Down) $ sumCoords renderCoords toRight
  mapM_ (renderChar_ '|' . RenderState) (leftWallCoords ++ rightWallCoords)

  -- lower wall
  _ <- renderStrLn (horizontalWall 'T') lowerLeft
  return $ RenderState worldCoords


render_ :: Char -> Coords -> WorldSize -> RenderState -> IO ()
render_ char worldCoords worldSize (RenderState renderCoords) =
  case location worldCoords worldSize of
    InsideWorld -> renderChar_ char loc
    _           -> return ()
  where loc = RenderState $ sumCoords renderCoords worldCoords
