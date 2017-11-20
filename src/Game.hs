{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Game(
        runGameWorker
      ) where

import           Imajuscule.Prelude

import           Data.List( minimumBy )
import           Data.Maybe( catMaybes
                           , isNothing )
import           Data.Text( pack )

import           Animation( Animation(..)
                          , quantitativeExplosionThenSimpleExplosion
                          , simpleExplosion
                          , renderAnimations
                          , mkAnimationTree
                          , mkAnimation )
import           Console( beginFrame
                        , endFrame )
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
import           GameParameters( GameParameters(..) )
import           Laser( LaserRay(..)
                      , shootLaserFromShip
                      , LaserType(..)
                      , LaserPolicy(..)
                      , mkLaserAnimation )
import           Level( Level(..)
                      , LevelFinished(..)
                      , renderLevel
                      , getEventForMaybeDeadline
                      , isLevelFinished
                      , MessageState(..)
                      , messageDeadline
                      , firstLevel
                      , lastLevel )
import           Number( Number(..)
                       , survivingNumbers
                       , showShotNumbers )
import           Render( RenderState
                       , mkRenderStateToCenterWorld
                       , translate
                       , Alignment(..)
                       , renderAlignedTxt
                       , RenderState
                       , go )
import           Space( Space(..)
                       , location
                       , renderSpace )
import           Timing( Timer(..)
                       , KeyTime(..)
                       , UTCTime
                       , getCurrentTime
                       , addGameStepDuration )
import           World( World(..)
                      , mkWorld
                      , BattleShip(..)
                      , nextWorld
                      , earliestAnimationDeadline
                      , accelerateShip
                      , moveWorld
                      , renderWorld )
import          WorldSize( WorldSize(..)
                         , worldSizeFromLevel )

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
  (GameState a b d world@(World balls _ (BattleShip (PosSpeed shipCoords _) ammo safeTime collisions) space animations) g target (Level i finished))
  te@(TimedEvent event t) =
   let (maybeLaserRayTheoretical, newAmmo) = if ammo > 0 then case event of
           (Action Laser dir) -> (LaserRay dir <$> shootLaserFromShip shipCoords dir Infinite (`location` space), pred ammo)
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

runGameWorker :: GameParameters -> IO ()
runGameWorker params = makeInitialState params firstLevel >>= loop params

makeInitialState :: GameParameters -> Int -> IO GameState
makeInitialState (GameParameters shape wallType) level = do
  let numbers = [1..(3+level)] -- more and more numbers as level increases
      worldSize = worldSizeFromLevel level shape
  coords <- mkRenderStateToCenterWorld worldSize
  world <- mkWorld worldSize wallType numbers
  t <- getCurrentTime
  return $ GameState (Timer t) (KeyTime t) coords world [] (sum numbers `quot` 2) $ Level level Nothing

loop :: GameParameters -> GameState -> IO ()
loop params state =
  updateGame params state >>= loop params

updateGame :: GameParameters -> GameState -> IO GameState
updateGame params state = getTimedEvent state >>= updateGameUsingTimedEvent params state

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

updateGameUsingTimedEvent :: GameParameters -> GameState -> TimedEvent -> IO GameState
updateGameUsingTimedEvent
 params
 state@(GameState a b d world f g h@(Level level mayLevelFinished))
 te@(TimedEvent event t) =
  case event of
    Nonsense -> return state
    StartLevel nextLevel -> makeInitialState params nextLevel
    _        -> do
      let newState = case event of
            (Timeout GameStep gt) -> GameState a (addGameStepDuration gt) d (moveWorld t world) f g h
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

      centerUp   = translate (Row $ -1)        (Col $ cHalf + 1) upperLeft
      centerDown = translate (Row $ rFull + 1) (Col $ cHalf + 1) upperLeft
      leftMiddle = translate (Row $ rHalf + 1) (Col $ -1)  upperLeft
  _ <- renderAlignedTxt Centered ("Level " <> pack (show level) <> " of " <> pack (show lastLevel)) centerDown
  _ <- go Down <$> renderAlignedTxt RightAligned ("[" <> pack (replicate ammo '.') <> "]") leftMiddle
       >>= renderAlignedTxt RightAligned (showShotNumbers shotNumbers)
  _ <- renderAlignedTxt Centered ("Objective : " <> pack (show target)) centerUp
  renderSpace space upperLeft >>=
    (\worldCorner -> do
      activeAnimations <- renderAnimations k (`location` space) worldCorner animations
      renderWorldAndLevel state worldCorner
      return activeAnimations)

renderWorldAndLevel :: GameState -> RenderState -> IO ()
renderWorldAndLevel (GameState _ _ _
                   world@(World _ _ _ (Space _ (WorldSize (Coords (Row rs) (Col cs))) _) _)
                   _ _ level) worldCorner = do
  renderWorld world worldCorner
  let
    rightMiddle = translate (Row (quot rs 2)) (Col $ cs + 2) worldCorner
  renderLevel level rightMiddle
