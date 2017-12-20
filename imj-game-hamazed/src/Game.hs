{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}

module Game(
        gameWorker
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
import           Game.Parameters
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

import           Physics.Discrete.Collision
import           Util


{-# INLINABLE gameWorker #-}
gameWorker :: (Draw e, MonadReader e m, MonadIO m)
           => m ()
gameWorker =
  getGameParameters >>= runGameWorker


{-# INLINABLE nextGameState #-}
nextGameState :: (Draw e, MonadReader e m, MonadIO m)
              => GameState m
              -> TimedEvent
              -> GameState m
nextGameState
  (GameState b world@(World _ _ ship@(BattleShip _ ammo _ _) space animations _) futureWorld
             g (Level i target finished) (WorldAnimation (WorldEvolutions j upDown left) k l))
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
  in assert (isFinished newAnim) $ GameState b newWorld futureWorld allShotNumbers newLevel newAnim


{-# INLINABLE outerSpaceAnims #-}
outerSpaceAnims :: (Draw e, MonadReader e m, MonadIO m)
                => KeyTime
                -> Space
                -> LaserRay Actual
                -> [BoundedAnimationUpdate m]
outerSpaceAnims keyTime@(KeyTime t) (Space _ sz _) ray@(LaserRay dir _) =
  let ae = afterEnd ray
  in case onFronteer ae sz of
        Just outDir -> let speed = assert (dir == outDir) (scalarProd 2 $ speed2vec $ coordsForDirection outDir)
                           explosions = explosionGravity speed $ translateInDir outDir ae
                       in map (((`BoundedAnimationUpdate` TerminalWindow) .
                                (\ (char, f) -> mkAnimationUpdate f keyTime WithZero (Speed 1) (Just char))) .
                                  ((,) $ niceChar $ getSeconds t))
                                    explosions
        Nothing -> []


{-# INLINABLE laserAnims #-}
laserAnims :: (Draw e, MonadReader e m, MonadIO m)
           => KeyTime
           -> LaserRay Actual
           -> [BoundedAnimationUpdate m]
laserAnims keyTime ray
 = [BoundedAnimationUpdate (mkLaserAnimationUpdate keyTime ray) WorldFrame]

replaceAnimations :: [BoundedAnimationUpdate m] -> GameState m -> GameState m
replaceAnimations anims (GameState c (World wa wb wc wd _ ew) b f g h) =
  GameState c (World wa wb wc wd anims ew) b f g h

nextDeadline :: GameState m -> UTCTime -> Maybe Deadline
nextDeadline s t =
  let l = getDeadlinesByDecreasingPriority s t
  in  overdueDeadline t l <|> earliestDeadline l

earliestDeadline :: [Deadline] -> Maybe Deadline
earliestDeadline [] = Nothing
earliestDeadline l  = Just $ minimumBy (\(Deadline t1 _) (Deadline t2 _) -> compare t1 t2 ) l

overdueDeadline :: UTCTime -> [Deadline] -> Maybe Deadline
overdueDeadline t = find (\(Deadline (KeyTime t') _) -> t' < t)

-- | priorities are : message > game forward > animation forward
getDeadlinesByDecreasingPriority :: GameState m -> UTCTime -> [Deadline]
getDeadlinesByDecreasingPriority s@(GameState _ _ _ _ level _) t =
  maybe
    (catMaybes [messageDeadline level t, getGameDeadline s, animationDeadline s])
    (: [])
      (worldAnimationDeadline s)

getGameDeadline :: GameState m -> Maybe Deadline
getGameDeadline (GameState nextGameStep _ _ _ (Level _ _ levelFinished) _) =
  maybe
    (maybe
      Nothing
      (\s -> Just $ Deadline s GameStep)
        nextGameStep)
    (const Nothing)
      levelFinished

animationDeadline :: GameState m -> Maybe Deadline
animationDeadline (GameState _ world _ _ _ _) =
  maybe Nothing (\ti -> Just $ Deadline ti AnimationStep) $ earliestAnimationDeadline world

worldAnimationDeadline :: GameState m -> Maybe Deadline
worldAnimationDeadline (GameState _ _ _ _ _ (WorldAnimation _ mayDeadline _)) =
  maybe
    Nothing
    (\deadline -> Just $ Deadline deadline FrameAnimationStep)
      mayDeadline


accelerateShip' :: Direction -> GameState m -> GameState m
accelerateShip' dir (GameState c (World wa wb ship wc wd we) b f g h) =
  let newShip = accelerateShip dir ship
      world = World wa wb newShip wc wd we
  in GameState c world b f g h


--------------------------------------------------------------------------------
-- IO
--------------------------------------------------------------------------------


{-# INLINABLE runGameWorker #-}
runGameWorker :: (Draw e, MonadReader e m, MonadIO m)
              => GameParameters
              -> m ()
runGameWorker params =
  mkInitialState params firstLevel Nothing
    >>= \case
      Left err -> error err
      Right ew -> loop params ew

mkInitialState :: (MonadIO m)
               => GameParameters
               -> Int
               -> Maybe (GameState m)
               -> m (Either String (GameState m))
mkInitialState (GameParameters shape wallType) levelNumber mayState = do
  let numbers = [1..(3+levelNumber)] -- more and more numbers as level increases
      target = sum numbers `quot` 2
      newLevel = Level levelNumber target Nothing
      newSize = worldSizeFromLevel levelNumber shape
      newAmmo = 10
      newShotNums = []
      make ew = do
        newWorld <- mkWorld ew newSize wallType numbers newAmmo
        t <- liftIO getCurrentTime
        let (curWorld, level, ammo, shotNums) =
              maybe
              (newWorld, newLevel, 0, [])
              (\(GameState _ w@(World _ _ (BattleShip _ curAmmo _ _) _ _ _)
                           _ curShotNums curLevel _) ->
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
        return $ Right $ GameState gameDeadline curWorld newWorld newShotNums newLevel worldAnimation
  mkEmbeddedWorld newSize >>= either (return . Left) make


{-# INLINABLE loop #-}
loop :: (Draw e, MonadReader e m, MonadIO m)
     => GameParameters
     -> GameState m
     -> m ()
loop params state =
  updateGame params state >>= (\(st, mayMeta) ->
    maybe (loop params st) (const $ return ()) mayMeta)


{-# INLINABLE updateGame #-}
updateGame :: (Draw e, MonadReader e m, MonadIO m)
           => GameParameters
           -> GameState m
           -> m (GameState m, Maybe Meta)
updateGame params state = do
  evt <- liftIO $ getTimedEvent state
  case evt of
    TimedEvent (Interrupt i) _ -> return (state, Just i)
    _ -> do
      st <- updateGameUsingTimedEvent params state evt
      return (st, Nothing)

getTimedEvent :: GameState m -> IO TimedEvent
getTimedEvent state =
  getEvent state >>= \evt -> do
    t <- getCurrentTime
    return $ TimedEvent evt t

getEvent :: GameState m -> IO Event
getEvent state@(GameState _ _ _ _ level _) = do
  t <- getCurrentTime
  let deadline = nextDeadline state t
  getEventForMaybeDeadline level deadline t


{-# INLINABLE updateGameUsingTimedEvent #-}
updateGameUsingTimedEvent :: (Draw e, MonadReader e m, MonadIO m)
                          => GameParameters
                          -> GameState m
                          -> TimedEvent
                          -> m (GameState m)
updateGameUsingTimedEvent
 params
 state@(GameState b world futWorld f h@(Level level target mayLevelFinished) i)
 te@(TimedEvent event t) =
  case event of
    Nonsense -> return state
    StartLevel nextLevel ->
      mkInitialState params nextLevel (Just state)
        >>= \case
              Left err -> error err
              Right s -> return s
    _ -> do
          let newState = case event of
                (Timeout FrameAnimationStep _) -> updateAnim t state
                (Timeout GameStep gt) -> GameState (Just $ addGameStepDuration gt) (moveWorld t world) futWorld f h i
                (Timeout MessageStep _) -> -- TODO this part is ugly, we should not have to deduce so much
                                           -- MessageStep is probably the wrong abstraction level
                  case mayLevelFinished of
                    Just (LevelFinished stop finishTime _) ->
                      let newLevel = Level level target (Just $ LevelFinished stop finishTime ContinueMessage)
                      in GameState b world futWorld f newLevel i
                    Nothing -> state
                _ -> state
          updateGame2 te newState


updateAnim :: UTCTime -> GameState m -> GameState m
updateAnim t (GameState _ curWorld futWorld j k (WorldAnimation evolutions _ it)) =
     let nextIt@(Iteration _ nextFrame) = nextIteration it
         (world, gameDeadline, worldAnimDeadline) =
            maybe
              (futWorld , Just $ KeyTime t, Nothing)
              (\dt ->
               (curWorld, Nothing         , Just $ KeyTime $ addUTCTime (floatSecondsToNominalDiffTime dt) t))
              $ getDeltaTime evolutions nextFrame
         wa = WorldAnimation evolutions worldAnimDeadline nextIt
     in GameState gameDeadline world futWorld j k wa


{-# INLINABLE updateGame2 #-}
updateGame2 :: (Draw e, MonadReader e m, MonadIO m)
            => TimedEvent
            -> GameState m
            -> m (GameState m)
updateGame2
 te@(TimedEvent event _)
 s@(GameState _ _ _ _ _ anim) =
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
      renderDrawing
      return $ replaceAnimations animations s2


{-# INLINABLE renderGame #-}
renderGame :: (Draw e, MonadReader e m, MonadIO m)
           => Maybe KeyTime
           -> GameState m
           -> m [BoundedAnimationUpdate m]
renderGame k (GameState _ world@(World _ _ _ space@(Space _ (WorldSize (Coords rs cs)) _)
                                         animations (EmbeddedWorld mayTermWindow curUpperLeft))
                        _ _ level wa) =
  renderSpace space curUpperLeft >>=
    (\worldCorner -> do
        activeAnimations <- renderAnimations k space mayTermWindow worldCorner animations
        -- TODO merge 2 functions below (and no need to pass worldCorner)
        renderWorld world
        renderLevelMessage level (translate' (quot rs 2) (cs + 2) worldCorner)
        renderWorldAnimation wa -- render it last so that when it animates
                                  -- to reduce, it goes over numbers and ship
        return activeAnimations)

{-# INLINABLE renderAnimations #-}
renderAnimations :: (Monad m)
                 => Maybe KeyTime
                 -> Space
                 -> Maybe (Window Int)
                 -> Coords
                 -> [BoundedAnimationUpdate m]
                 -> m [BoundedAnimationUpdate m]
renderAnimations k space mayTermWindow worldCorner animations = do
  let renderAnimation (BoundedAnimationUpdate a@(AnimationUpdate _ _ _ render) f) = do
        let interaction = locationFunction f space mayTermWindow worldCorner
                           >>> \case
                                InsideWorld  -> Stable
                                OutsideWorld -> Mutation
        fmap (`BoundedAnimationUpdate` f) <$> render k a interaction worldCorner
  activeAnimations <- mapM renderAnimation animations
  let res = catMaybes activeAnimations
  return res
