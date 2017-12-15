{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Game(
        runGameWorker
      ) where

import           Imajuscule.Prelude

import           Data.List( minimumBy, find )
import           Data.Maybe( catMaybes )
import           Data.String(String)

import           Animation
import           Animation.Design.Chars

import           Game.Color
import           Game.Deadline( Deadline(..) )
import           Game.Types
import           Game.Event
import           Game.Level
import           Game.Parameters( GameParameters(..) )
import           Game.Render
import           Game.World
import           Game.World.Embedded
import           Game.World.Evolution
import           Game.World.Laser
import           Game.World.Number
import           Game.World.Ship
import           Game.World.Size
import           Game.World.Space

import           Geo.Conversion
import           Geo.Continuous
import           Geo.Discrete

import           Util


nextGameState :: GameState -> TimedEvent -> GameState
nextGameState
  (GameState a b world@(World _ _ ship@(BattleShip _ ammo _ _) space animations _) futureWorld
             g (Level i target finished) (WorldAnimation (WorldEvolutions j upDown left) k l) rf)
  te@(TimedEvent event t) =
  let (remainingBalls, destroyedBalls, maybeLaserRay, newAmmo) = withLaserAction event world

      keyTime = KeyTime t

      outerSpaceAnims_ =
         if null destroyedBalls
           then
             maybe [] (outerSpaceAnims keyTime space) maybeLaserRay
           else
            []

      newAnimations =
            destroyedNumbersAnimations keyTime event destroyedBalls
         ++ shipAnims ship event
         ++ maybe [] (laserAnims keyTime) maybeLaserRay
         ++ outerSpaceAnims_
         ++ animations

      newWorld = nextWorld world remainingBalls newAmmo newAnimations
      destroyedNumbers = map (\(Number _ n) -> n) destroyedBalls
      allShotNumbers = g ++ destroyedNumbers
      newLeft =
        if null destroyedNumbers && ammo == newAmmo
          then
            left
          else
            let frameSpace = mkFrameSpec worldFrameColors world
                infos = mkLeftInfo Normal newAmmo allShotNumbers
            in mkTextAnimLeft frameSpace frameSpace infos 0 -- 0 duration, since animation is over anyway
      newFinished = finished <|> isLevelFinished newWorld (sum allShotNumbers) target te
      newLevel = Level i target newFinished
      newAnim = WorldAnimation (WorldEvolutions j upDown newLeft) k l
  in assert (isFinished newAnim) $ GameState a b newWorld futureWorld allShotNumbers newLevel newAnim rf


outerSpaceAnims :: KeyTime -> Space -> LaserRay Actual -> [BoundedAnimation]
outerSpaceAnims keyTime@(KeyTime t) (Space _ sz _) ray@(LaserRay dir _) =
  let ae = afterEnd ray
  in case onFronteer ae sz of
        Just outDir -> let speed = assert (dir == outDir) (scalarProd 2 $ speed2vec $ coordsForDirection outDir)
                           explosions = explosionGravity speed $ translateInDir outDir ae
                       in map (((`BoundedAnimation` TerminalWindow) .
                                (\ (char, f) -> mkAnimation f keyTime WithZero (Speed 1) (Just char))) .
                                  ((,) $ niceChar $ getSeconds t))
                                    explosions
        Nothing -> []


laserAnims :: KeyTime -> LaserRay Actual -> [BoundedAnimation]
laserAnims keyTime ray
 = [BoundedAnimation (mkLaserAnimation keyTime ray) WorldFrame]

replaceAnimations :: [BoundedAnimation] -> GameState -> GameState
replaceAnimations anims (GameState a c (World wa wb wc wd _ ew) b f g h rf) =
  GameState a c (World wa wb wc wd anims ew) b f g h rf

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
getDeadlinesByDecreasingPriority s@(GameState _ _ _ _ _ level _ _) t =
  maybe
    (catMaybes [messageDeadline level t, getGameDeadline s, animationDeadline s])
    (: [])
      (worldAnimationDeadline s)

getGameDeadline :: GameState -> Maybe Deadline
getGameDeadline (GameState _ nextGameStep _ _ _ (Level _ _ levelFinished) _ _) =
  maybe
    (maybe
      Nothing
      (\s -> Just $ Deadline s GameStep)
        nextGameStep)
    (const Nothing)
      levelFinished

animationDeadline :: GameState -> Maybe Deadline
animationDeadline (GameState _ _ world _ _ _ _ _) =
  maybe Nothing (\ti -> Just $ Deadline ti AnimationStep) $ earliestAnimationDeadline world

worldAnimationDeadline :: GameState -> Maybe Deadline
worldAnimationDeadline (GameState _ _ _ _ _ _ (WorldAnimation _ mayDeadline _) _) =
  maybe
    Nothing
    (\deadline -> Just $ Deadline deadline FrameAnimationStep)
      mayDeadline


accelerateShip' :: Direction -> GameState -> GameState
accelerateShip' dir (GameState a c (World wa wb ship wc wd we) b f g h rf) =
  let newShip = accelerateShip dir ship
      world = World wa wb newShip wc wd we
  in GameState a c world b f g h rf


--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------

runGameWorker :: RenderFunctions -> GameParameters -> IO ()
runGameWorker renderFunctions params =
  mkInitialState renderFunctions params firstLevel Nothing
    >>= \case
      Left err -> error err
      Right ew -> loop params ew

mkInitialState :: RenderFunctions -> GameParameters -> Int -> Maybe GameState -> IO (Either String GameState)
mkInitialState rf (GameParameters shape wallType) levelNumber mayState = do
  let numbers = [1..(3+levelNumber)] -- more and more numbers as level increases
      target = sum numbers `quot` 2
      newLevel = Level levelNumber target Nothing
      newSize = worldSizeFromLevel levelNumber shape
      newAmmo = 10
      newShotNums = []
      make ew = do
        newWorld <- mkWorld ew newSize wallType numbers newAmmo
        t <- getCurrentTime
        let (curWorld, level, ammo, shotNums) =
              maybe
              (newWorld, newLevel, 0, [])
              (\(GameState _ _ w@(World _ _ (BattleShip _ curAmmo _ _) _ _ _)
                           _ curShotNums curLevel _ _) ->
                  (w, curLevel, curAmmo, curShotNums))
                mayState
            curInfos = mkInfos Normal ammo shotNums level
            newInfos = mkInfos ColorAnimated newAmmo newShotNums newLevel
            worldAnimation =
              mkWorldAnimation
                (mkFrameSpec worldFrameColors curWorld, curInfos)
                (mkFrameSpec worldFrameColors newWorld, newInfos)
                t
            gameDeadline =
              if isFinished worldAnimation
                then
                  Just $ KeyTime t
                else
                  Nothing
        return $ Right $ GameState (Timer t) gameDeadline curWorld newWorld newShotNums newLevel worldAnimation rf
  eew <- mkEmbeddedWorld newSize
  either (return . Left) make eew


loop :: GameParameters -> GameState -> IO ()
loop params state =
  updateGame params state >>= (\(st, mayMeta) ->
    maybe (loop params st) (const $ return ()) mayMeta)

updateGame :: GameParameters -> GameState -> IO (GameState, Maybe Meta)
updateGame params state =
  getTimedEvent state >>=
    \evt -> case evt of
      TimedEvent (Interrupt i) _ -> return (state, Just i)
      _ -> do
        st <- updateGameUsingTimedEvent params state evt
        return (st, Nothing)

getTimedEvent :: GameState -> IO TimedEvent
getTimedEvent state =
  getEvent state >>= \evt -> do
    t <- getCurrentTime
    return $ TimedEvent evt t

getEvent :: GameState -> IO Event
getEvent state@(GameState _ _ _ _ _ level _ _) = do
  t <- getCurrentTime
  let deadline = nextDeadline state t
  getEventForMaybeDeadline level deadline t


updateGameUsingTimedEvent :: GameParameters -> GameState -> TimedEvent -> IO GameState
updateGameUsingTimedEvent
 params
 state@(GameState a b world futWorld f h@(Level level target mayLevelFinished) i rf)
 te@(TimedEvent event t) =
  case event of
    Nonsense -> return state
    StartLevel nextLevel ->
      mkInitialState rf params nextLevel (Just state)
        >>= \case
              Left err -> error err
              Right s -> return s
    _ -> do
          let newState = case event of
                (Timeout FrameAnimationStep _) -> updateAnim t state
                (Timeout GameStep gt) -> GameState a (Just $ addGameStepDuration gt) (moveWorld t world) futWorld f h i rf
                (Timeout MessageStep _) -> -- TODO this part is ugly, we should not have to deduce so much
                                           -- MessageStep is probably the wrong abstraction level
                  case mayLevelFinished of
                    Just (LevelFinished stop finishTime _) ->
                      let newLevel = Level level target (Just $ LevelFinished stop finishTime ContinueMessage)
                      in GameState a b world futWorld f newLevel i rf
                    Nothing -> state
                _ -> state
          updateGame2 te newState


updateAnim :: UTCTime -> GameState -> GameState
updateAnim t (GameState a _ curWorld futWorld j k (WorldAnimation evolutions _ it) rf) =
     let nextIt@(Iteration (_, nextFrame)) = nextIteration it
         (world, gameDeadline, worldAnimDeadline) =
            maybe
              (futWorld , Just $ KeyTime t, Nothing)
              (\dt ->
               (curWorld, Nothing         , Just $ KeyTime $ addUTCTime (floatSecondsToNominalDiffTime dt) t))
              $ getDeltaTime evolutions nextFrame
         wa = WorldAnimation evolutions worldAnimDeadline nextIt
     in GameState a gameDeadline world futWorld j k wa rf

updateGame2 :: TimedEvent -> GameState -> IO GameState
updateGame2
 te@(TimedEvent event _)
 s@(GameState _ _ _ _ _ _ anim (RenderFunctions _ _ _ flush)) =
  case event of
    Action Ship dir -> return $ accelerateShip' dir s
    _ -> do
      let s2 =
            if isFinished anim
              then
                nextGameState s te
              else
                s
      animations <- renderGame (getKeyTime event) s2
      flush
      return $ replaceAnimations animations s2

renderGame :: Maybe KeyTime -> GameState -> IO [BoundedAnimation]
renderGame k (GameState _ _ world@(World _ _ _ space@(Space _ (WorldSize (Coords rs cs)) _)
                                         animations (EmbeddedWorld mayTermWindow curUpperLeft))
                        _ _ level wa rf@(RenderFunctions renderChar renderChars _ _)) =
  renderSpace space curUpperLeft renderChars >>=
    (\worldCorner -> do
        activeAnimations <- renderAnimations k space mayTermWindow worldCorner renderChar animations
        -- TODO merge 2 functions below (and no need to pass worldCorner)
        renderWorld rf world
        renderLevelMessage rf level (translate' (quot rs 2) (cs + 2) worldCorner)
        renderWorldAnimation rf wa -- render it last so that when it animates
                                  -- to reduce, it goes over numbers and ship
        return activeAnimations)

renderAnimations :: Maybe KeyTime
                 -> Space
                 -> Maybe (Window Int)
                 -> Coords
                 -> (Char -> Coords -> LayeredColor -> IO ())
                 -> [BoundedAnimation]
                 -> IO [BoundedAnimation]
renderAnimations k space mayTermWindow worldCorner b animations = do
  let renderAnimation (BoundedAnimation a@(Animation _ _ _ render) f) = do
        let fLocation = locationFunction f space mayTermWindow worldCorner
        fmap (`BoundedAnimation` f) <$> render k a fLocation worldCorner b
  activeAnimations <- mapM renderAnimation animations
  let res = catMaybes activeAnimations
  return res
