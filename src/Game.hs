{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Game(
        runGameWorker
      ) where

import           Imajuscule.Prelude

import           Data.Char( intToDigit )
import           Data.List( minimumBy, find )
import           Data.Maybe( catMaybes
                           , isNothing )
import           Data.Text( pack )

import           Animation.Types
import           Animation( simpleExplosion
                          , gravityExplosionThenSimpleExplosion
                          , animatedNumber
                          , mkAnimationTree
                          , mkAnimation )
import           Animation.Design.Chars(niceChar)
import           Animation.RenderUpdate
import           Console( beginFrame
                        , endFrame )
import           Deadline( Deadline(..) )
import           Geo( Col(..)
                    , Coords(..)
                    , speed2vec
                    , coordsForDirection
                    , Direction(..)
                    , sumVec2d, sumCoords
                    , scalarProd
                    , PosSpeed(..)
                    , Row(..)
                    , Vec2(..) )
import           Event( Event(..)
                      , TimedEvent(..)
                      , Step(..)
                      , Meta(..)
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
import           Render( RenderState, EmbeddedWorld(..), Window(..)
                       , mkEmbeddedWorld
                       , translate
                       , Alignment(..)
                       , renderAlignedTxt
                       , RenderState(..)
                       , go )
import           Space( Space(..)
                       , location
                       , renderSpace )
import           Timing
import           Util( getSeconds )
import           World
import           WorldSize

data GameState = GameState {
    _gameStateStartTime :: !Timer
  , _gameStateNextMotionStep :: !KeyTime
  , _gameStateEmbeddedWorld :: !EmbeddedWorld
  , _gameStateworld :: !World
  , _gameStateShotNumbers :: ![Int]
  , _gameStateTarget :: !Int
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

       keyTime = KeyTime t
       char = niceChar $ getSeconds t
       tree = mkAnimationTree shipCoords
       newAnimations =
         destroyNumbersAnimations destroyedBalls event keyTime
         ++ case event of
              Timeout GameStep k -> [BoundedAnimation (mkAnimation (simpleExplosion 8 (tree $ ReboundAnd Stop)) k (Speed 2) char) TerminalWindow | not (null collisions) && isNothing safeTime]
--              Explosion resolution -> [mkAnimation (simpleExplosion resolution (tree $ ReboundAnd Stop)) keyTime (Speed 2) char]
--              GravityExplosion -> [mkAnimation (gravityExplosion (Vec2 1.0 (-1.0)) (tree $ ReboundAnd $ ReboundAnd Stop)) keyTime (Speed 2) char]
              _ -> []
         ++ maybe [] (\ray -> [BoundedAnimation (mkLaserAnimation keyTime ray) WorldFrame]) maybeLaserRay
         ++ animations

       newWorld = nextWorld world remainingBalls newAmmo newAnimations
       destroyedNumbers = map (\(Number _ n) -> n) destroyedBalls
       allShotNumbers = g ++ destroyedNumbers
       newFinished = finished <|> isLevelFinished newWorld (sum allShotNumbers) target te
       newLevel = Level i newFinished
   in  GameState a b d newWorld allShotNumbers target newLevel

destroyNumbersAnimations :: [Number] -> Event -> KeyTime -> [BoundedAnimation]
destroyNumbersAnimations nums event keyTime =
  let sp = case event of
        (Action Laser dir) -> speed2vec $ coordsForDirection dir
        _ -> Vec2 0 0
      variations = [ Vec2 0.3     (-0.4)
                   , Vec2 (-0.55) (-0.29)
                   , Vec2 (-0.1)  0.9
                   , Vec2 1.2     0.2]

      speeds = map (sumVec2d (scalarProd 2 sp)) variations
      --animation (Number (PosSpeed pos _) n) = quantitativeExplosionThenSimpleExplosion (min 6 n) (mkAnimationTree pos)
      anim pos speedLaser = gravityExplosionThenSimpleExplosion speedLaser (mkAnimationTree pos (ReboundAnd $ ReboundAnd Stop))
      animation pos = map ((\f -> (f, Speed 2)) . anim pos) speeds
  in case nums of
    Number (PosSpeed pos _) n:_ ->
      let animations = animation pos ++ [(animatedNumber n (mkAnimationTree pos Traverse), Speed 1)]
      in  map (\(f,speed) -> BoundedAnimation (mkAnimation f keyTime speed $ intToDigit n) WorldFrame) animations
    _ -> []

replaceAnimations :: [BoundedAnimation] -> GameState -> GameState
replaceAnimations anims (GameState a c e (World wa wb wc wd _) f g h) =
  GameState a c e (World wa wb wc wd anims) f g h

nextDeadline :: GameState -> UTCTime -> Maybe Deadline
nextDeadline s t =
  let l = getDeadlinesByDecreasingPriority s t
  in  overdueDeadline t l <|> earliestDeadline l

earliestDeadline :: [Deadline] -> Maybe Deadline
earliestDeadline [] = Nothing
earliestDeadline l  = Just $ minimumBy (\(Deadline t1 _) (Deadline t2 _) -> compare t1 t2 ) l

overdueDeadline :: UTCTime -> [Deadline] -> Maybe Deadline
overdueDeadline t = find (\(Deadline (KeyTime t') _) -> t' < t)

-- | priorities are : message > game forward > animation forward
getDeadlinesByDecreasingPriority :: GameState -> UTCTime -> [Deadline]
getDeadlinesByDecreasingPriority s@(GameState _ _ _ _ _ _ level) t =
  catMaybes [messageDeadline level t, gameDeadline s, animationDeadline s]

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
  ew <- mkEmbeddedWorld worldSize
  world <- mkWorld worldSize wallType numbers
  t <- getCurrentTime
  return $ GameState (Timer t) (KeyTime t) ew world [] (sum numbers `quot` 2) $ Level level Nothing

loop :: GameParameters -> GameState -> IO ()
loop params state =
  updateGame params state >>= (\(st, mayMeta) ->
    maybe (loop params st) (const $ return ()) mayMeta)

updateGame :: GameParameters -> GameState -> IO (GameState, Maybe Meta)
updateGame params state = getTimedEvent state >>= (\evt ->
  case evt of
    TimedEvent (Interrupt i) _ -> return (state, Just i)
    _                 -> do
      st <- updateGameUsingTimedEvent params state evt
      return (st, Nothing))

getTimedEvent :: GameState -> IO TimedEvent
getTimedEvent state =
  getEvent state >>= \evt -> do
    t <- getCurrentTime
    return $ TimedEvent evt t

getEvent :: GameState -> IO Event
getEvent state@(GameState _ _ _ _ _ _ level) = do
  t <- getCurrentTime
  let deadline = nextDeadline state t
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


renderGame :: Maybe KeyTime -> GameState -> IO [BoundedAnimation]
renderGame k state@(GameState _ _ (EmbeddedWorld mayTermWindow upperLeft)
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
    (\worldCorner@(RenderState wcc) -> do
        activeAnimations <- mapM (\(BoundedAnimation a f) -> do
          let worldLocation = (`location` space)
              terminalLocation (Window h w) coordsInWorld =
                let (Coords (Row r) (Col c)) = sumCoords coordsInWorld wcc
                in if r >= 0 && r < h && c >= 0 && c < w
                     then
                       InsideWorld
                     else
                       OutsideWorld
              fLocation = case f of
                WorldFrame -> worldLocation
                TerminalWindow -> maybe worldLocation terminalLocation mayTermWindow
          fmap (`BoundedAnimation` f) <$>
            renderAndUpdateAnimation k fLocation worldCorner a) animations
        renderWorldAndLevel state worldCorner
        let res = catMaybes activeAnimations
        return res)

renderWorldAndLevel :: GameState -> RenderState -> IO ()
renderWorldAndLevel (GameState _ _ _
                   world@(World _ _ _ (Space _ (WorldSize (Coords (Row rs) (Col cs))) _) _)
                   _ _ level) worldCorner = do
  renderWorld world worldCorner
  let
    rightMiddle = translate (Row (quot rs 2)) (Col $ cs + 2) worldCorner
  renderLevel level rightMiddle
