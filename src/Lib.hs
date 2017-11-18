{-# LANGUAGE LambdaCase #-}

module Lib
    ( run
    ) where

import           Imajuscule.Prelude

import           Control.Applicative( (<|>) )
import           Control.Exception( assert
                                  , finally )
import           Control.Monad( when )

import           Data.Char( intToDigit )
import           Data.List( minimumBy
                          , partition )
import           Data.Maybe( catMaybes
                           , isJust
                           , isNothing
                           , maybe )
import           Data.Time( UTCTime
                          , addUTCTime
                          , diffUTCTime
                          , getCurrentTime )

import qualified System.Console.Terminal.Size as Terminal(size
                                                         , Window(..))
import           System.IO( getChar )
import           System.Timeout( timeout )

import           Animation( quantitativeExplosionThenSimpleExplosion
                          , simpleExplosion
                          , simpleLaser
                          , renderAnimations
                          , mkAnimationTree )
import           Console( ColorIntensity(..)
                        , Color(..)
                        , configureConsoleFor
                        , ConsoleConfig(..)
                        , beginFrame
                        , endFrame
                        , setForeground
                        , renderChar_
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
import           Space( Space(..)
                      , getMaterial
                      , location
                      , Material(..) )
import           Threading( runAndWaitForTermination
                          , Termination(..) )
import           Timing( addMotionStepDuration
                       --, computeTime
                       --, showUpdateTick
                       , Timer(..)
                       , KeyTime(..)
                       , diffTimeSecToMicros )
import           Util( showListOrSingleton )
import           World( accelerateShip
                      , Event(..)
                      , TimedEvent(..)
                      , ActionTarget(..)
                      , getKeyTime
                      , Animation(..)
                      , BattleShip(..)
                      , earliestAnimationDeadline
                      , moveWorld
                      , mkAnimation
                      , mkWorld
                      , nextWorld
                      , Step(..)
                      , World(..)
                      , Number(..) )
import           WorldSize( WorldSize(..) )


--------------------------------------------------------------------------------
-- Pure
--------------------------------------------------------------------------------

data GameState = GameState {
    _startTime :: !Timer
  , _nextMotionStep :: !KeyTime
  , _upperLeftCorner :: !Coords
  , _world :: !World
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

eventFromChar :: Level -> Char -> Event
eventFromChar (Level n finished) char = case finished of
  Nothing -> eventFromCharInGame char
  Just (LevelFinished stop _ ContinueMessage) ->
    case stop of
      Won      -> if n < lastLevel then StartLevel (succ n) else EndGame
      (Lost _) -> StartLevel firstLevel
  _ -> Nonsense -- between level end and proposal to continue

eventFromCharInGame :: Char -> Event
eventFromCharInGame c = case c of
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
  'd' -> Action Ship Down
  'e' -> Action Ship Up
  's' -> Action Ship LEFT
  'f' -> Action Ship RIGHT
  _   -> Nonsense


nextGameState :: GameState -> TimedEvent -> GameState
nextGameState
 (GameState a b d world@(World balls _ (BattleShip (PosSpeed shipCoords _) ammo safeTime collisions) sz animations) g target (Level i finished))
 te@(TimedEvent event t) =
  let (maybeLaserRayTheoretical, newAmmo) = if ammo > 0 then case event of
          (Action Laser dir) -> (LaserRay dir <$> shootLaserFromShip shipCoords dir Infinite sz, pred ammo)
          _     -> (Nothing, ammo)
        else (Nothing, ammo)
      ((remainingBalls, destroyedBalls), maybeLaserRay) = maybe ((balls,[]), Nothing) (survivingNumbers balls RayDestroysFirst) maybeLaserRayTheoretical
      destroyedNumbers = map (\(Number _ n) -> n) destroyedBalls
      allShotNumbers = g ++ destroyedNumbers
      animation (Number (PosSpeed pos _) n) = quantitativeExplosionThenSimpleExplosion (max 6 n) (mkAnimationTree pos)
      keyTime = KeyTime t
      newAnimations = (case destroyedBalls of
        n:_ -> mkAnimation (animation n) keyTime : animations
        _ -> animations)
        ++ case event of
            Timeout GameStep k -> [mkAnimation (simpleExplosion (mkAnimationTree shipCoords)) k | not (null collisions) && isNothing safeTime]
            _ -> []
        ++ maybe [] (\(LaserRay laserDir (Ray laserSeg)) -> [mkAnimation (simpleLaser laserSeg (laserChar laserDir)) keyTime]) maybeLaserRay
      newWorld = nextWorld world remainingBalls newAmmo newAnimations
      newFinished = finished <|> computeFinished newWorld (sum allShotNumbers) target te
      newLevel = Level i newFinished
  in  GameState a b d newWorld allShotNumbers target newLevel

computeFinished :: World -> Int -> Int -> TimedEvent -> Maybe LevelFinished
computeFinished (World _ _ (BattleShip _ ammo safeTime collisions) _ _) sumNumbers target (TimedEvent lastEvent t) =
    maybe Nothing (\stop -> Just $ LevelFinished stop t InfoMessage) allChecks
  where
    allChecks = checkShipCollision <|> checkSum <|> checkAmmo

    checkShipCollision = case lastEvent of
      (Timeout GameStep _) ->
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
replaceAnimations anims (GameState a c e (World wa wb wc wd _) f g h) =
  GameState a c e (World wa wb wc wd anims) f g h

data Deadline = Deadline {
    _deadlineTime :: !KeyTime
  , _deadlineType :: !Step
} deriving(Eq, Show)

earliestDeadline :: GameState -> UTCTime -> Maybe Deadline
earliestDeadline s t =
  let l = listDeadlines s t
  in  if null l then Nothing else Just $ minimumBy (\(Deadline t1 _) (Deadline t2 _) -> compare t1 t2 ) l

listDeadlines :: GameState -> UTCTime -> [Deadline]
listDeadlines s@(GameState _ _ _ _ _ _ level) t = catMaybes [gameDeadline s, animationDeadline s, messageDeadline level t]

gameDeadline :: GameState -> Maybe Deadline
gameDeadline (GameState _ nextGameStep _ _ _ _ (Level _ levelFinished)) =
  maybe (Just $ Deadline nextGameStep GameStep) (const Nothing) levelFinished

animationDeadline :: GameState -> Maybe Deadline
animationDeadline (GameState _ _ _ world _ _ _) =
  maybe Nothing (\ti -> Just $ Deadline ti AnimationStep) $ earliestAnimationDeadline world

messageDeadline :: Level -> UTCTime -> Maybe Deadline
messageDeadline (Level _ mayLevelFinished) t =
  maybe Nothing
  (\(LevelFinished _ timeFinished messageType) ->
      case messageType of
        InfoMessage ->
          let finishedSinceSeconds = diffUTCTime t timeFinished
              delay = 2
              nextMessageStep = addUTCTime (delay - finishedSinceSeconds) t
          in  Just $ Deadline (KeyTime nextMessageStep) MessageStep
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
  termSize <- Terminal.size
  let numbers = [1..(3+level)] -- more and more numbers as level increases
      s = 36 + 2 * (1-level) -- less and less space as level increases
      -- we need even world dimensions to ease level construction
      worldSize = WorldSize $ Coords (Row $ assert (even s) s) (Col (2*s))
      coords = maybe (Coords (Row 3) (Col 3)) (`worldUpperLeftFromTermSize` worldSize) termSize
  world <- mkWorld worldSize numbers
  t <- getCurrentTime
  return $ GameState (Timer t) (KeyTime t) coords world [] (sum numbers `quot` 2) $ Level level Nothing

worldUpperLeftFromTermSize :: Terminal.Window Int -> WorldSize -> Coords
worldUpperLeftFromTermSize (Terminal.Window h w) (WorldSize (Coords (Row rs) (Col cs))) =
  let walls = 2 :: Int
  in Coords (Row $ quot (h-(rs+walls)) 2) (Col $ quot (w-(cs+walls)) 2)

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
updateGame state = getTimedEvent state >>= updateGameUsingTimedEvent state

updateGameUsingTimedEvent :: GameState -> TimedEvent -> IO GameState
updateGameUsingTimedEvent
 state@(GameState a b d world f g h@(Level level mayLevelFinished))
 te@(TimedEvent event t) =
  case event of
    Nonsense -> return state
    StartLevel nextLevel -> makeInitialState nextLevel
    _        -> do
      let newState = case event of
            (Timeout GameStep gt) -> GameState a (addMotionStepDuration gt) d (moveWorld t world) f g h
            (Action Frame dir)  -> GameState a b (sumCoords d $ coordsForDirection dir) world f g h
            (Timeout MessageStep _) -> -- TODO this part is ugly, we should not have to deduce so much
                                     -- MessageStep is probably the wrong abstraction level
              case mayLevelFinished of
                Just (LevelFinished stop finishTime _) -> GameState a b d world f g $ Level level (Just $ LevelFinished stop finishTime ContinueMessage)
                Nothing -> state
            _ -> state
      updateGame2 te newState

updateGame2 :: TimedEvent -> GameState -> IO GameState
updateGame2 te@(TimedEvent event _) s =
  case event of
    Action Ship dir -> return $ accelerateShip' dir s
    _ -> do
      beginFrame
      let s2 = nextGameState s te
      animations <- renderGame (getKeyTime event) s2
      endFrame
      return $ replaceAnimations animations s2


accelerateShip' :: Direction -> GameState -> GameState
accelerateShip' dir (GameState a c d (World wa wb ship wc wd) f g h) =
  let newShip = accelerateShip dir ship
      world = World wa wb newShip wc wd
  in GameState a c d world f g h


getTimedEvent :: GameState -> IO TimedEvent
getTimedEvent state =
  getEvent state >>= \evt -> do
    t <- getCurrentTime
    return $ TimedEvent evt t

getEvent :: GameState -> IO Event
getEvent state@(GameState _ _ _ _ _ _ level) = do
  t <- getCurrentTime
  case earliestDeadline state t of
    (Just (Deadline k@(KeyTime deadline) deadlineType)) -> do
      let
        timeToDeadlineMicros = diffTimeSecToMicros $ diffUTCTime deadline t
      getEventWithinDurationMicros level timeToDeadlineMicros k deadlineType
    Nothing -> eventFromChar level <$> getChar

getEventWithinDurationMicros :: Level -> Int -> KeyTime -> Step -> IO Event
getEventWithinDurationMicros level durationMicros k step =
  (\case
    Nothing   -> Timeout step k
    Just char -> eventFromChar level char
    ) <$> getCharWithinDurationMicros durationMicros

getCharWithinDurationMicros :: Int -> IO (Maybe Char)
getCharWithinDurationMicros durationMicros =
  if durationMicros < 0
    then return Nothing
    else timeout durationMicros getChar

renderGame :: Maybe KeyTime -> GameState -> IO [Animation]
renderGame k state@(GameState _ _ upperLeft
                   (World _ _ (BattleShip _ ammo _ _) space@(Space _ (WorldSize (Coords (Row rs) (Col cs))) _) animations)
                   shotNumbers target (Level level _)) = do
  --printTimer state
  let r = RenderState upperLeft
      addWallSize = (+ 2)
      half = flip quot 2
      mkSizes s = (addWallSize s, half s)
      (rFull, rHalf) = mkSizes rs
      (_    , cHalf) = mkSizes cs

      centerUp   = translate (Coords (Row $ -1)        (Col cHalf)) upperLeft
      centerDown = translate (Coords (Row $ rFull + 1) (Col cHalf)) upperLeft
      leftMiddle = translate (Coords (Row rHalf)       (Col $ -1)) upperLeft
  _ <- renderCentered ("Level " ++ show level ++ " of " ++ show lastLevel) $ RenderState centerDown
  _ <- goDown <$> renderRightAligned ("[" ++ replicate ammo '.' ++ "]") (RenderState leftMiddle)
       >>= renderRightAligned (showShotNumbers shotNumbers)
  _ <- renderCentered ("Objective : " ++ show target) (RenderState centerUp)
  renderWorldFrame space r >>=
    (\worldCorner -> do
      activeAnimations <- renderAnimations k (`location` space) worldCorner animations
      renderWorld state worldCorner
      return activeAnimations)

-- TODO remove LaserRay from GameState
renderWorld :: GameState -> RenderState -> IO ()
renderWorld (GameState _ _ _
                   (World balls _ (BattleShip (PosSpeed shipCoords _) _ safeTime collisions) sz@(Space _ (WorldSize (Coords (Row rs) (Col cs))) _) _)
                   _ _ (Level level levelState)) worldCorner@(RenderState upperLeft) = do
  -- render numbers, including the ones that will be destroyed, if any
  mapM_ (\(Number (PosSpeed pos _) i) -> render_ (intToDigit i) pos sz worldCorner) balls
  when (null collisions) (do
    let shipColor = if isNothing safeTime then Blue else Red
    setForeground Vivid shipColor
    render_ '+' shipCoords sz worldCorner
    setForeground Vivid White)
  let
    rightMiddle = translate (Coords (Row (quot rs 2)) (Col $ cs + 2)) upperLeft
  mapM_ (renderLevelState (RenderState rightMiddle) level) levelState

renderLevelState :: RenderState -> Int -> LevelFinished -> IO ()
renderLevelState (RenderState coords) level (LevelFinished stop _ messageState) = do
  let color = case stop of
        (Lost _) -> Yellow
        Won      -> Green
      topLeft = RenderState $ translateInDir RIGHT coords
  setForeground Vivid color
  afterFirst <- renderStrLn (case stop of
    (Lost reason) -> "You Lose (" ++ reason ++ ")"
    Won           -> "You Win!") topLeft
  setForeground Vivid White
  when (messageState == ContinueMessage) $ do
    let from = goDown afterFirst
    renderStrLn_ (if level == lastLevel
      then "You reached the end of the game! Hit Ctrl + C to quit."
      else
        let action = case stop of
                          (Lost _) -> "restart"
                          Won      -> "continue"
        in "Hit a key to " ++ action ++ " ...") from
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

-- TODO precompute the list of Str in the Space to avoid recreating them at each frame
-- and allow a better rendering ( T | _ + ) depnding on neighbours
renderWorldFrame :: Space -> RenderState -> IO RenderState
renderWorldFrame (Space _ (WorldSize (Coords (Row rs) (Col cs))) renderedWorld) upperLeft@(RenderState upperLeftCoords) = do
  let horizontalWall = replicate (cs + 2)
      lowerLeft = RenderState $ sumCoords upperLeftCoords $ Coords (Row $ rs+1) (Col 0)

  -- upper wall
  (RenderState renderCoords) <- renderStrLn (horizontalWall '_') upperLeft
  let worldCoords = translateInDir RIGHT renderCoords

  -- left & right walls
  let leftWallCoords = take rs $ iterate (translateInDir Down) renderCoords
      toRight = Coords (Row 0) (Col $ cs+1)
      rightWallCoords = take rs $ iterate (translateInDir Down) $ sumCoords renderCoords toRight
  mapM_ (renderChar_ '|' . RenderState) (leftWallCoords ++ rightWallCoords)

  -- lower wall
  renderStrLn_ (horizontalWall 'T') lowerLeft

  mapM_ (\(r, str) -> renderStrLn_ str $ RenderState $ sumCoords worldCoords $ Coords (Row r) (Col 0))$ zip [0..] renderedWorld

  return $ RenderState worldCoords

render_ :: Char -> Coords -> Space -> RenderState -> IO ()
render_ char worldCoords space (RenderState renderCoords) =
  case getMaterial worldCoords space of
    Air  -> renderChar_ char loc
    Wall -> return ()
  where loc = RenderState $ sumCoords renderCoords worldCoords
