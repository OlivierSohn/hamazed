
module Game(
        gameWorker
      ) where

import           Imajuscule.Prelude

import           Control.Exception( assert )
import           Control.Monad( when )

import           Data.Char( intToDigit )

import           Data.List( minimumBy )
import           Data.Maybe( catMaybes
                           , isNothing )

import           Animation( Animation(..)
                          , quantitativeExplosionThenSimpleExplosion
                          , simpleExplosion
                          , renderAnimations
                          , mkAnimationTree
                          , mkAnimation )
import           Console( ColorIntensity(..)
                        , Color(..)
                        , beginFrame
                        , endFrame
                        , setForeground
                        , renderChar_
                        , renderStr
                        , renderStr_ )
import           Deadline( Deadline(..) )
import           Geo( Col(..)
                    , Coords(..)
                    , Direction(..)
                    , PosSpeed(..)
                    , Row(..) )
import           Event( Event(..)
                      , TimedEvent(..)
                      , Step(..)
                      , ActionTarget(..)
                      , getKeyTime )
import           Laser( LaserRay(..)
                      , shootLaserFromShip
                      , LaserType(..)
                      , LaserPolicy(..)
                      , mkLaserAnimation )
import           Level( Level(..)
                      , LevelFinished(..)
                      , getEventForMaybeDeadline
                      , isLevelFinished
                      , MessageState(..)
                      , messageDeadline
                      , GameStops(..)
                      , firstLevel
                      , lastLevel )
import           Number( Number(..)
                       , survivingNumbers )
import           Render( RenderState
                       , mkRenderStateToCenterWorld
                       , translate
                       , renderChar
                       , Alignment(..)
                       , renderAlignedStr
                       , RenderState
                       , go
                       , move )
import           Space( Space(..)
                     , getMaterial
                     , location
                     , Material(..) )
import           Timing( Timer(..)
                       , KeyTime(..)
                       , UTCTime
                       , getCurrentTime
                       , addMotionStepDuration )
import           World( World(..)
                      , mkWorld
                      , BattleShip(..)
                      , nextWorld
                      , earliestAnimationDeadline
                      , accelerateShip
                      , moveWorld )
import          WorldSize( WorldSize(..)
                         , mkWorldSize
                         , Height(..)
                         , Width(..) )

data GameState = GameState {
    _startTime :: !Timer
  , _nextMotionStep :: !KeyTime
  , _upperLeftCorner :: !RenderState
  , _world :: !World
  , _gameShotNumbers :: ![Int]
  , _gameTarget :: !Int
  , _gameStateLevel :: !Level
}

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
         ++ maybe [] (\ray -> [mkLaserAnimation keyTime ray]) maybeLaserRay
       newWorld = nextWorld world remainingBalls newAmmo newAnimations
       newFinished = finished <|> isLevelFinished newWorld (sum allShotNumbers) target te
       newLevel = Level i newFinished
   in  GameState a b d newWorld allShotNumbers target newLevel

replaceAnimations :: [Animation] -> GameState -> GameState
replaceAnimations anims (GameState a c e (World wa wb wc wd _) f g h) =
  GameState a c e (World wa wb wc wd anims) f g h

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


accelerateShip' :: Direction -> GameState -> GameState
accelerateShip' dir (GameState a c d (World wa wb ship wc wd) f g h) =
  let newShip = accelerateShip dir ship
      world = World wa wb newShip wc wd
  in GameState a c d world f g h


--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

gameWorker :: IO ()
gameWorker = makeInitialState firstLevel >>= loop


makeInitialState :: Int -> IO GameState
makeInitialState level = do
  let numbers = [1..(3+level)] -- more and more numbers as level increases
      s = 36 + 2 * (1-level) -- less and less space as level increases
      -- we need even world dimensions to ease level construction
      worldSize = mkWorldSize (Height $ assert (even s) s) (Width (2*s))
  coords <- mkRenderStateToCenterWorld worldSize
  world <- mkWorld worldSize numbers
  t <- getCurrentTime
  return $ GameState (Timer t) (KeyTime t) coords world [] (sum numbers `quot` 2) $ Level level Nothing

loop :: GameState -> IO ()
loop state =
  updateGame state >>= loop

updateGame :: GameState -> IO GameState
updateGame state = getTimedEvent state >>= updateGameUsingTimedEvent state

getTimedEvent :: GameState -> IO TimedEvent
getTimedEvent state =
  getEvent state >>= \evt -> do
    t <- getCurrentTime
    return $ TimedEvent evt t

getEvent :: GameState -> IO Event
getEvent state@(GameState _ _ _ _ _ _ level) = do
  t <- getCurrentTime
  let deadline = earliestDeadline state t
  getEventForMaybeDeadline level deadline t

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



renderGame :: Maybe KeyTime -> GameState -> IO [Animation]
renderGame k state@(GameState _ _ upperLeft
                   (World _ _ (BattleShip _ ammo _ _) space@(Space _ (WorldSize (Coords (Row rs) (Col cs))) _) animations)
                   shotNumbers target (Level level _)) = do
  let addWallSize = (+ 2)
      half = flip quot 2
      mkSizes s = (addWallSize s, half s)
      (rFull, rHalf) = mkSizes rs
      (_    , cHalf) = mkSizes cs

      centerUp   = translate (Row $ -1)        (Col cHalf) upperLeft
      centerDown = translate (Row $ rFull + 1) (Col cHalf) upperLeft
      leftMiddle = translate (Row rHalf)       (Col $ -1)  upperLeft
  _ <- renderAlignedStr Centered ("Level " ++ show level ++ " of " ++ show lastLevel) centerDown
  _ <- go Down <$> renderAlignedStr RightAligned ("[" ++ replicate ammo '.' ++ "]") leftMiddle
       >>= renderAlignedStr RightAligned (showShotNumbers shotNumbers)
  _ <- renderAlignedStr Centered ("Objective : " ++ show target) centerUp
  renderWorldFrame space upperLeft >>=
    (\worldCorner -> do
      activeAnimations <- renderAnimations k (`location` space) worldCorner animations
      renderWorld state worldCorner
      return activeAnimations)

renderWorld :: GameState -> RenderState -> IO ()
renderWorld (GameState _ _ _
                   (World balls _ (BattleShip (PosSpeed shipCoords _) _ safeTime collisions) space@(Space _ (WorldSize (Coords (Row rs) (Col cs))) _) _)
                   _ _ (Level level levelState)) worldCorner = do
  -- render numbers, including the ones that will be destroyed, if any
  mapM_ (\(Number (PosSpeed pos _) i) -> renderIfNotColliding (intToDigit i) pos space worldCorner) balls
  when (null collisions) (do
    let shipColor = if isNothing safeTime then Blue else Red
    setForeground Vivid shipColor
    renderIfNotColliding '+' shipCoords space worldCorner
    setForeground Vivid White)
  let
    rightMiddle = translate (Row (quot rs 2)) (Col $ cs + 2) worldCorner
  mapM_ (renderLevelState rightMiddle level) levelState

renderLevelState :: RenderState -> Int -> LevelFinished -> IO ()
renderLevelState coords level (LevelFinished stop _ messageState) = do
  let color = case stop of
        (Lost _) -> Yellow
        Won      -> Green
      topLeft = go RIGHT coords
  setForeground Vivid color
  afterFirst <- renderStr (case stop of
    (Lost reason) -> "You Lose (" ++ reason ++ ")"
    Won           -> "You Win!") topLeft
  setForeground Vivid White
  when (messageState == ContinueMessage) $
    renderStr_ (if level == lastLevel
      then "You reached the end of the game! Hit Ctrl + C to quit."
      else
        let action = case stop of
                          (Lost _) -> "restart"
                          Won      -> "continue"
        in "Hit a key to " ++ action ++ " ...") (go Down afterFirst)

showShotNumbers :: [Int] -> String
showShotNumbers nums =
  "[" ++ unwords (map show nums) ++ "]"

renderWorldFrame :: Space -> RenderState -> IO RenderState
renderWorldFrame (Space _ (WorldSize (Coords (Row rs) (Col cs))) renderedWorld) upperLeft = do
  let horizontalWall = replicate (cs + 2)
      lowerLeft = move (rs+1) Down upperLeft

  -- upper wall
  renderState <- renderStr (horizontalWall '_') upperLeft
  let worldCoords = go RIGHT renderState

  -- left & right walls
  let leftWallCoords = take rs $ iterate (go Down) renderState
      rightWallCoords = take rs $ iterate (go Down) $ move (cs+1) RIGHT renderState
  mapM_ (renderChar_ '|') (leftWallCoords ++ rightWallCoords)

  -- lower wall
  renderStr_ (horizontalWall 'T') lowerLeft

  -- world
  mapM_ (\(r, str) -> renderStr_ str (move r Down worldCoords)) $ zip [0..] renderedWorld

  return worldCoords

renderIfNotColliding :: Char -> Coords -> Space -> RenderState -> IO ()
renderIfNotColliding char worldCoords space r =
  case getMaterial worldCoords space of
    Air  -> renderChar char worldCoords r
    Wall -> return ()
