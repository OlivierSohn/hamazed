{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}

module Imj.Game.Hamazed.Logic
    ( HamazedGame(..)
    , checkComponentStatus
    , checkAllComponentStatus
    , checkSums
    , checkTargetAndAmmo
    , countAmmo
    , countLiveAmmo
    , getLevelOutcome
    , putLevelOutcome
    , addParticleSystems
    , envFunctions
    , putViewMode
    , putWorld
    , putAnimation
    , getViewMode
    , getWorld
    , mkGameStateEssence
    , moveWorld
    , laserEventAction
    ) where

import           Imj.Prelude
import           Prelude(length)

import           Control.Concurrent(forkIO)
import           Control.Concurrent.Async(withAsync)
import           Control.Monad.Reader.Class(MonadReader, asks)
import           Control.Monad.State.Class(MonadState)
import           Control.Monad.IO.Class(MonadIO)
import           Data.Attoparsec.Text(decimal, endOfInput, skipSpace)
import           Data.Char(intToDigit)
import qualified Data.IntSet as ISet
import qualified Data.List as List
import           Data.List(elem)
import           Data.Maybe(isJust)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Text(pack, unpack)
import qualified Data.Text as Text

import           Imj.ClientView.Types
import qualified Imj.Data.Tree as Tree(toList)
import           Imj.Game.Hamazed.Level.Types
import           Imj.Game.Priorities
import           Imj.Game.Hamazed.Loop.Event.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.Network.Internal.Types
import           Imj.Game.Hamazed.State.Types
import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Graphics.Class.HasSizedFace
import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.ParticleSystem.Design.Types
import           Imj.Graphics.UI.Animation.Types
import           Imj.Input.Types
import           Imj.Network
import           Imj.Server.Class

import           Imj.Control.Concurrent.AsyncGroups.Class
import           Imj.Event
import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.Infos
import           Imj.Game.Hamazed.Sound
import           Imj.Game.Hamazed.World.Create
import           Imj.Game.Hamazed.World.Draw
import           Imj.Game.Hamazed.World.Space.Draw
import           Imj.Game.Hamazed.World.Space
import           Imj.Game.Timing
import           Imj.Game.Update
import           Imj.GameItem.Weapon.Laser
import           Imj.Geo.Continuous
import           Imj.Graphics.Color.Types
import           Imj.Graphics.Font
import           Imj.Graphics.ParticleSystem
import           Imj.Graphics.Screen
import           Imj.Graphics.Text.ColorString hiding(putStrLn)
import           Imj.Graphics.Text.Render
import           Imj.Graphics.UI.Animation
import           Imj.Graphics.UI.Chat
import           Imj.Graphics.UI.Colored
import           Imj.Graphics.UI.RectContainer
import           Imj.Music hiding(Do)
import           Imj.Physics.Discrete.Collision
import           Imj.Random.MWC.Parallel(mkOneGenPerCapability)
import           Imj.Sums


{-| 'HamazedGame' has two 'World's : during 'Level' transitions,
we draw the /old/ 'World' while using the dimensions of the /new/ 'World'
to animate the UI accordingly. -}
data HamazedGame = HamazedGame {
    currentWorld :: !World
  , mayFutureWorld :: !(Maybe World)
    -- ^ Maybe the world that we transition to (when a level is over).
    -- Once the transition is over, we replace 'currentWorld' with this 'Just' value.
  , _gameStateShotNumbers :: ![ShotNumber]
    -- ^ Which 'Number's were shot
  , getGameLevel :: !Level
    -- ^ The current 'Level'
  , getUIAnimation :: !UIAnimation
    -- ^ Inter-level animation.
  , getViewMode' :: {-unpack sum-} !ViewMode
}

uiAnimationDeadline :: UIAnimation -> Maybe Deadline
uiAnimationDeadline =
  maybe
    Nothing
    (\deadline -> Just $ Deadline deadline animateUIPriority AnimateUI)
    . getUIAnimationDeadline

instance GameLogic HamazedGame where
  type ServerT        HamazedGame = HamazedServer
  type ClientOnlyEvtT HamazedGame = HamazedEvent

  cmdParser cmd = case cmd of
    "color" -> tryReport <|> tryCmd
    _ -> fail $ "'" <> unpack cmd <> "' is an unknown command."
   where
    tryReport = do
      void endOfInput
      return $ Right $ Report $ Get ColorSchemeCenterKey
    tryCmd = do
      r <- decimal
      skipSpace
      g <- decimal
      skipSpace
      b <- decimal
      return $ Do . Put . ColorSchemeCenter <$> userRgb r g b

  initialGame = liftIO . initialGameState initialParameters CenterSpace

  onResizedWindow _ =
    gets game >>= \(Game _ (Screen _ screenCenter) g@(HamazedGame curWorld mayNewWorld _ _ uiAnimation _) _ _ _ _ _ _ _) -> do
      let sizeSpace = getSize $ getWorldSpace $ fromMaybe curWorld mayNewWorld
          newPosition = upperLeftFromCenterAndSize screenCenter sizeSpace
          newAnim = setPosition newPosition uiAnimation
      putGameState $ g { getUIAnimation = newAnim }

  onPlayersChanged = updateShipsText

  {-# INLINABLE onUpdateUIAnim #-}
  onUpdateUIAnim = updateUIAnim

  {-# INLINABLE onCustomEvent #-}
  onCustomEvent = hamazedEvtUpdate

  {-# INLINABLE getViewport #-}
  getViewport (Screen _ center) (HamazedGame world _ _ _ _ mode) =
      let offset = getWorldOffset mode world
          worldCorner = getWorldCorner world center offset
      in flip RectContainer (sumCoords (Coords (-1) (-1)) worldCorner) $ getSize $ getWorldSpace world

  {-# INLINABLE getDeadlines #-}
  getDeadlines (HamazedGame _ _ _ _ uiAnim _) =
    maybeToList $ uiAnimationDeadline uiAnim

  {-# INLINABLE drawGame #-}
  drawGame = gets game >>=
    \(Game _ (Screen _ screenCenter) (HamazedGame world@(World _ _ _ renderedSpace _) _ _ _ wa mode)
             animations _ _ _ _ _ _) -> do
      let offset = getWorldOffset mode world
          worldCorner = getWorldCorner world screenCenter offset
      -- draw the walls outside the matrix:
      fill (materialGlyph Wall) outerWallsColors
      -- draw the matrix:
      drawSpace renderedSpace worldCorner
      mapM_ (\(Prioritized _ a) -> drawSystem a worldCorner) animations
      drawWorld world worldCorner
      drawUIAnimation offset wa -- draw animation after the world so that when it morphs
                                -- it goes over numbers and ship
      flip RectContainer (sumCoords (Coords (-1) (-1)) worldCorner) . getSize . getWorldSpace <$> getWorld

  keyMaps key val = fmap CliEvt <$> (case val of
    Excluded -> return Nothing
    Setup -> return $ case key of
      AlphaNum c -> case c of
        ' ' -> Just $ ClientAppEvt $ ExitedState Setup
        '1' -> Just $ OnCommand $ Do $ Put $ WorldShape Square
        '2' -> Just $ OnCommand $ Do $ Put $ WorldShape Rectangle'2x1
        --'e' -> Just $ OnCommand $ Do $ Put $ WallDistribution None
        --'r' -> Just $ OnCommand $ Do $ Put $ WallDistribution $ minRandomBlockSize 0.5
        'y' -> Just $ OnCommand $ Do $ Succ BlockSize
        'g' -> Just $ OnCommand $ Do $ Pred BlockSize
        'u' -> Just $ OnCommand $ Do $ Succ WallProbability
        'h' -> Just $ OnCommand $ Do $ Pred WallProbability
        _ -> Nothing
      _ -> Nothing
    PlayLevel status -> case status of
      Running -> maybe
        (case key of
          AlphaNum c -> case c of
            'k' -> Just $ ClientAppEvt $ Action Laser Down
            'i' -> Just $ ClientAppEvt $ Action Laser Up
            'j' -> Just $ ClientAppEvt $ Action Laser LEFT
            'l' -> Just $ ClientAppEvt $ Action Laser RIGHT
            'd' -> Just $ ClientAppEvt $ Action Ship Down
            'e' -> Just $ ClientAppEvt $ Action Ship Up
            's' -> Just $ ClientAppEvt $ Action Ship LEFT
            'f' -> Just $ ClientAppEvt $ Action Ship RIGHT
            --'r'-> Just $ Evt ToggleEventRecording
            _   -> Nothing
          _ -> Nothing)
        (const Nothing)
        <$> getLevelOutcome
      WhenAllPressedAKey _ (Just _) _ -> return Nothing
      WhenAllPressedAKey x Nothing havePressed ->
        (maybe
          Nothing
          (maybe
            (error "logic")
            (bool (Just $ ClientAppEvt $ CanContinue x) Nothing)
            . flip Map.lookup havePressed)) <$> getMyId
      New -> return Nothing
      Paused _ _ -> return Nothing
      Countdown _ _ -> return Nothing
      OutcomeValidated _ -> return Nothing
      CancelledNoConnectedPlayer -> return Nothing
      WaitingForOthersToEndLevel _ -> return Nothing)

mkGameStateEssence :: WorldId -> HamazedGame -> Maybe GameStateEssence
mkGameStateEssence wid' (HamazedGame curWorld mayNewWorld shotNums (Level levelEssence _) _ _) =
  let (essence, mayWid) = worldToEssence $ fromMaybe curWorld mayNewWorld
  in maybe
    Nothing
    (\wid -> bool Nothing (Just $ GameStateEssence essence shotNums levelEssence) $ wid == wid')
    mayWid

initialGameState :: WorldParameters
                 -> ViewMode
                 -> Screen
                 -> IO HamazedGame
initialGameState _ mode s =
  mkInitialState mkEmptyLevelEssence mkMinimalWorldEssence Nothing mempty mode s Nothing

mkInitialState :: (MonadIO m)
               => LevelEssence
               -> WorldEssence
               -> Maybe WorldId
               -> Map ShipId Player
               -> ViewMode
               -> Screen
               -> Maybe HamazedGame
               -> m HamazedGame
mkInitialState = mkIntermediateState []

mkIntermediateState :: (MonadIO m)
                    => [ShotNumber]
                    -> LevelEssence
                    -> WorldEssence
                    -> Maybe WorldId
                    -> Map ClientId Player
                    -> ViewMode
                    -> Screen
                    -> Maybe HamazedGame
                    -> m HamazedGame
mkIntermediateState newShotNums newLevel essence wid names mode (Screen _ screenCenter) mayState = do
  let newWorld@(World _ _ space' _ _) = mkWorld essence wid
      (curWorld@(World _ _ curSpace _ _), level, shotNums) =
        maybe
        (newWorld, newLevel, [])
        (\(HamazedGame w _ curShotNums (Level curLevel _) _ _) ->
            (w, curLevel, curShotNums))
          mayState
      curInfos = mkInfos Normal        (getWorldShips curWorld) names shotNums    level
      newInfos = mkInfos ColorAnimated (getWorldShips newWorld) names newShotNums newLevel
      (horizontalDist, verticalDist) = computeViewDistances mode
  kt <- liftIO getSystemTime
  let uiAnimation =
        mkUIAnimation
          (Colored worldFrameColors
          $ mkRectContainerWithCenterAndInnerSize screenCenter (getSize curSpace), curInfos)
          (Colored worldFrameColors
          $ mkRectContainerWithCenterAndInnerSize screenCenter (getSize space'), newInfos)
          horizontalDist verticalDist kt
          -- only when UIAnimation is over, curWorld will be replaced by newWorld.
          -- during UIAnimation, we need the two worlds.
  return $ HamazedGame curWorld (Just newWorld) newShotNums (mkLevel newLevel) uiAnimation mode


{-# INLINABLE hamazedEvtUpdate #-}
hamazedEvtUpdate :: (GameLogicT e ~ HamazedGame
                   , MonadState (AppState HamazedGame) m
                   , MonadReader e m, Client e, HasSizedFace e, AsyncGroups e
                   , MonadIO m)
                   => CustomUpdateEvent HamazedGame
                   -> m ()
hamazedEvtUpdate (Right cliEvt) = case cliEvt of
  Interrupt Help -> error "not implemented"
  PlayProgram i -> liftIO $ playAtTempo (Wind i) 120 [notes| vdo vsol do sol ^do|]
hamazedEvtUpdate (Left srvEvt) = case srvEvt of
  PlayMusic music instr -> liftIO $ play music instr
  WorldRequest wid arg -> case arg of
    GetGameState ->
      mkGameStateEssence wid <$> getGameState >>= sendToServer . CurrentGameState wid
    Build dt spec ->
      asks sendToServer' >>= \send -> asks belongsTo' >>= \ownedByRequest ->
        void $ liftIO $ forkIO $ flip withAsync (`ownedByRequest` (fromIntegral wid)) $
          -- According to benchmarks, it is best to set the number of capabilities to the number of /physical/ cores,
          -- and to have no more than one worker per capability.
          mkOneGenPerCapability >>= \gens -> do
            let go = do
                  deadline <- addDuration dt <$> liftIO getSystemTime
                  -- TODO getSystemTime can be costly... instead, we should have a thread that queries time every second,
                  -- and atomicModifyIORef an IORef Bool. this same IORef Bool can be used to cancel the async gracefully.
                  -- But we should also read the IORef in the inner loop of matrix transformations to ensure prompt finish.
                  let continue = getSystemTime >>= \t -> return (t < deadline)
                  mkWorldEssence spec continue gens >>= \res -> do
                    send $ ClientAppEvt $ uncurry (WorldProposal wid) res
                    case fst res of
                      NeedMoreTime{} -> go
                      _ -> return ()
            go
    Cancel -> asks cancel' >>= \cancelAsyncsOwnedByRequest -> cancelAsyncsOwnedByRequest (fromIntegral wid)
  ChangeLevel levelEssence worldEssence wid ->
    gets game >>= \(Game _ screen state@(HamazedGame _ _ _ _ _ viewMode) _ _ names _ _ _ _) ->
      mkInitialState levelEssence worldEssence (Just wid) names viewMode screen (Just state)
        >>= putGameState
  PutGameState (GameStateEssence worldEssence shotNums levelEssence) wid ->
    gets game >>= \(Game _ screen state@(HamazedGame _ _ _ _ _ viewMode) _ _ names _ _ _ _) ->
      mkIntermediateState shotNums levelEssence worldEssence (Just wid) names viewMode screen (Just state)
        >>= putGameState
  GameEvent (PeriodicMotion accelerations shipsLosingArmor) ->
    onMove accelerations shipsLosingArmor
  GameEvent (LaserShot dir shipId) -> do
    liftIO laserSound
    onLaser shipId dir Add
  MeetThePlayers eplayers -> do
    sendToServer $ ExitedState Excluded
    putClientState $ ClientState Over Excluded
    let p = Map.map mkPlayer eplayers
    putPlayers p
    stateChat $ addMessage $ ChatMessage $ welcome p
  GameInfo notif ->
    stateChat $ addMessage $ ChatMessage $ toTxt' notif
  EnterState s ->
    putClientState $ ClientState Ongoing s
  ExitState s  ->
    putClientState $ ClientState Over s

 where

  toTxt' (LevelResult (LevelNumber n) (Lost reason)) =
    colored ("- Level " <> pack (show n) <> " was lost : " <> reason <> ".") chatMsgColor
  toTxt' (LevelResult (LevelNumber n) Won) =
    colored ("- Level " <> pack (show n) <> " was won!") chatWinColor
  toTxt' GameWon =
    colored "- The game was won! Congratulations!" chatWinColor
  toTxt' (CannotCreateLevel errs n) =
    colored ( Text.intercalate "\n" errs <> "\nHence, the server cannot create level " <> pack (show n)) red

{-# INLINABLE onLaser #-}
onLaser :: (GameLogicT e ~ HamazedGame
          , MonadState (AppState HamazedGame) m
          , MonadReader e m, Client e
          , MonadIO m)
        => ShipId
        -> Direction
        -> Operation
        -> m ()
onLaser ship dir op =
  (liftIO getSystemTime >>= laserEventAction ship dir) >>= onDestroyedNumbers
 where
  onDestroyedNumbers (destroyedBalls, ammoChanged) =
    getGameState >>= \(HamazedGame w@(World _ ships _ _ _) f g (Level level@(LevelEssence _ target _) finished) a m) -> do
      let allShotNumbers = g ++ map (flip ShotNumber op . getNumber . getNumEssence) (Map.elems destroyedBalls)
          finishIfNoAmmo = checkTargetAndAmmo (countAmmo $ Map.elems ships) (applyOperations $ reverse allShotNumbers) target
          newFinished = finished <|> finishIfNoAmmo
          newLevel = Level level newFinished
      maybe
        (return ())
        (when (isNothing finished) . sendToServer . LevelEnded)
        newFinished
      putGameState $ HamazedGame w f allShotNumbers newLevel a m
      updateShipsText
      when ammoChanged checkSums

{-# INLINABLE onMove #-}
onMove :: (GameLogicT e ~ HamazedGame
         , MonadState (AppState HamazedGame) m
         , MonadReader e m, Client e
         , MonadIO m)
       => Map ShipId (Coords Vel)
       -> Set ShipId
       -> m ()
onMove accelerations shipsLosingArmor = do
  moveWorld accelerations shipsLosingArmor
  onHasMoved

{-# INLINABLE onHasMoved #-}
onHasMoved :: (GameLogicT e ~ HamazedGame
             , MonadState (AppState HamazedGame) m
             , MonadReader e m, Client e
             , MonadIO m)
           => m ()
onHasMoved =
  liftIO getSystemTime >>= shipParticleSystems >>= addParticleSystems >> getGameState
    >>= \(HamazedGame world@(World balls ships _ _ _) f shotNums (Level level@(LevelEssence _ target _) finished) anim m) -> do
      let oneShipAlive = any (shipIsAlive . getShipStatus) ships
          allCollisions = Set.unions $ mapMaybe
            (\(BattleShip _ _ shipStatus collisions _) ->
              case shipStatus of
                Armored -> Nothing
                _ -> Just collisions)
            $ Map.elems ships
          remainingBalls = Map.withoutKeys balls allCollisions
          numbersChanged =
            assert (Set.size allCollisions + Map.size remainingBalls == Map.size balls)
              $ not $ Set.null allCollisions
          newWorld = world { getWorldNumbers = remainingBalls }
          finishIfAllShipsDestroyed
            | oneShipAlive = Nothing
            | Set.null allCollisions = Nothing
            | otherwise =
                let nums = map (getNumber . getNumEssence) $ Map.elems $ Map.restrictKeys balls allCollisions
                in Just $ Lost $ "collision with " <> showListOrSingleton nums
          finishIfNoAmmo = checkTargetAndAmmo (countAmmo $ Map.elems ships) (applyOperations $ reverse shotNums) target
          newFinished = finished <|> finishIfAllShipsDestroyed <|> finishIfNoAmmo
          newLevel = Level level finished
      maybe
        (return ())
        (when (isNothing finished) . sendToServer . LevelEnded)
        newFinished
      putGameState $ assert (isFinished anim) $ HamazedGame newWorld f shotNums newLevel anim m
      when numbersChanged checkSums
      updateShipsText

-- | When the animation is over, swaps the future world with the current one.
{-# INLINABLE updateUIAnim #-}
updateUIAnim :: (GameLogicT e ~ HamazedGame -- TODO split
               , MonadState (AppState HamazedGame) m
               , MonadReader e m, Client e
               , MonadIO m)
             => Time Point System -> m ()
updateUIAnim t =
  getGameState >>= \(HamazedGame curWorld mayFutWorld j k a@(UIAnimation evolutions (UIAnimProgress _ it)) m) -> do
    let nextIt@(Iteration _ nextFrame) = nextIteration it
        (world, futWorld, worldAnimDeadline) = maybe
          (fromMaybe
            (error "ongoing UIAnimation with no future world")
            mayFutWorld
          , Nothing
          , Nothing)
          (\dt -> (curWorld, mayFutWorld, Just $ addDuration dt t))
          $ getDeltaTime evolutions nextFrame
        anims = a { getProgress = UIAnimProgress worldAnimDeadline nextIt }
    putGameState $ HamazedGame world futWorld j k anims m
    when (isFinished anims) $ do
      checkAllComponentStatus
      checkSums
      maybe (return ()) (sendToServer . IsReady) $ getId world


{- | If the ship is colliding and not in "safe time", and the event is a gamestep,
this function creates an animation where the ship and the colliding number explode.

The ship 'ParticleSystem' will have the initial speed of the number and vice-versa,
to mimic the rebound due to the collision. -}
shipParticleSystems :: (MonadState (AppState HamazedGame) m)
                    => Time Point System
                    -> m [Prioritized ParticleSystem]
shipParticleSystems k =
  getWorld >>= \w -> do
    envFuncs <- envFunctions $ WorldScope Air
    let sps _ (BattleShip _ _ Armored   _  _) = return []
        sps _ (BattleShip _ _ Unarmored _  _) = return []
        sps shipId (BattleShip (PosSpeed shipCoords shipSpeed) _ Destroyed collisions _)
          | Set.null collisions = return []
          | otherwise = getPlayer shipId >>= maybe
              (return [])
              (\(Player _ _ (PlayerColors _ cycles)) -> do
                -- when number and ship explode, they exchange speeds
                let collidingNumbersAvgSpeed =
                      Map.foldl'
                        (\s -> sumCoords s . getSpeed . getNumPosSpeed . getNumEssence)
                        zeroCoords
                        $ Map.restrictKeys (getWorldNumbers w) collisions
                    numSpeed = scalarProd 0.4 $ speed2vec collidingNumbersAvgSpeed
                    shipSpeed2 = scalarProd 0.4 $ speed2vec shipSpeed
                    k' = systemTimePointToParticleSystemTimePoint k
                    n = maybe '?' (intToDigit . getNumber . getNumEssence)
                      $ fmap snd $ Map.lookupMin $ Map.restrictKeys (getWorldNumbers w) collisions
                    color i =
                      cycleColors sumFrameParticleIndex $
                        if even i
                          then outer1 cycles
                          else wall2 cycles
                return $ List.map (Prioritized particleSystDefaultPriority)
                  $ fragmentsFreeFallThenExplode numSpeed shipCoords color (gameGlyph '|') (Speed 1) envFuncs k' ++
                    fragmentsFreeFallThenExplode shipSpeed2 shipCoords color (gameGlyph n) (Speed 1) envFuncs k')
    List.concat <$> Map.traverseWithKey sps (getWorldShips w)


{-# INLINABLE updateShipsText #-}
updateShipsText :: (MonadState (AppState HamazedGame) m)
                => m ()
updateShipsText =
  gets game >>= \(Game _ screen g@(HamazedGame (World _ ships _ _ _) _ shotNumbers (Level level _)
                    (UIAnimation (UIEvolutions j upDown _) p) mode ) _ _ names _ _ _ _) -> do
    let newLeft =
          let (horizontalDist, verticalDist) = computeViewDistances mode
              vp = getViewport screen g
              (_, _, leftMiddle, _) = getSideCenters $ mkRectContainerAtDistance vp horizontalDist verticalDist
              infos = mkLeftInfo Normal ships names shotNumbers level
          in mkTextAnimRightAligned leftMiddle leftMiddle infos 1 (fromSecs 1)
        newAnim = UIAnimation (UIEvolutions j upDown newLeft) p -- TODO use mkUIAnimation to have a smooth transition
    putAnimation newAnim


-- | Moves elements of game logic ('Number's, 'BattleShip').
--
-- Note that 'ParticleSystem's are not updated.
moveWorld :: MonadState (AppState HamazedGame) m
          => Map ShipId (Coords Vel)
          -> Set ShipId
          -> m ()
moveWorld accelerations shipsLosingArmor = getWorld >>= \(World balls ships space' rs f) -> do
  let newBalls =
        Map.map (\n@(Number e@(NumberEssence ps _ _) colors _) ->
                            n{ getNumEssence = e{ getNumPosSpeed = updateMovableItem space' ps }
                             , getNumColor = case colors of
                                 _:rest -> rest -- move forward in color animation
                                 [] -> [] })
        balls
      moveShip comps sid (BattleShip (PosSpeed prevPos oldSpeed) ammo status _ i) =
        let collisions
              | shipIsAlive status = Map.keysSet $ getColliding pos newBalls
              | otherwise = Set.empty

            newComps = comps ++ [ i | not $ Set.null collisions]

            destroyedOr x
              | Set.null collisions = x
              | otherwise = Destroyed

            newStatus = case status of
              Destroyed -> Destroyed
              Armored ->
                if sid `elem` shipsLosingArmor
                  then
                    destroyedOr Unarmored
                  else
                    Armored
              Unarmored -> destroyedOr Unarmored
            newSpeed =
              maybe oldSpeed (sumCoords oldSpeed) $ Map.lookup sid accelerations
            newPosSpeed@(PosSpeed pos _) = updateMovableItem space' $ PosSpeed prevPos newSpeed
        in (newComps, BattleShip newPosSpeed ammo newStatus collisions i)
      (changedComponents, newShips) = Map.mapAccumWithKey moveShip [] ships
  putWorld $ World newBalls newShips space' rs f
  mapM_ checkComponentStatus changedComponents

-- | Computes the effect of a laser shot on the 'World'.
laserEventAction :: (MonadState (AppState HamazedGame) m)
                 => ShipId
                 -> Direction
                 -- ^ The direction of the laser shot
                 -> Time Point System
                 -> m (Map NumId Number, Bool)
                 -- ^ 'Number's destroyed + ammo changed
laserEventAction shipId dir t =
  getWorld >>= \(World balls ships space' rs e) -> do
    let ship@(BattleShip (PosSpeed shipCoords _) ammo status _ component) = findShip shipId ships
        (maybeLaserRayTheoretical, newAmmo) =
          if ammo > 0 && shipIsAlive status
            then
              (Just $ shootLaserWithOffset shipCoords dir Infinite (`location` space')
             , pred ammo)
            else
              (Nothing
             , ammo)
        ((remainingBalls, destroyedBalls), maybeLaserRay) =
           maybe
             ((balls,Map.empty), Nothing)
             (\r -> Just <$> computeActualLaserShot balls (getPos . getNumPosSpeed . getNumEssence)
                                                    r DestroyFirstObstacle)
               maybeLaserRayTheoretical
        newShip = ship { getAmmo = newAmmo }
        newShips = Map.insert shipId newShip ships
    putWorld $ World remainingBalls newShips space' rs e

    let tps = systemTimePointToParticleSystemTimePoint t
    outerSpaceParticleSystems_ <-
      if Map.null destroyedBalls
        then
          maybe (return []) (outerSpaceParticleSystems tps shipId) maybeLaserRay
        else
          return []
    newSystems <- destroyedNumbersParticleSystems tps shipId dir destroyedBalls
    laserSystems <- maybe (return []) (laserParticleSystems tps shipId) maybeLaserRay
    addParticleSystems $ List.concat [newSystems, laserSystems, outerSpaceParticleSystems_]

    when (countLiveAmmo newShip == 0) $ checkComponentStatus component
    return (destroyedBalls, isJust maybeLaserRay)


destroyedNumbersParticleSystems :: (MonadState (AppState HamazedGame) m)
                                => Time Point ParticleSyst
                                -> ShipId
                                -> Direction -- ^ 'Direction' of the laser shot
                                -> Map NumId Number
                                -> m [Prioritized ParticleSystem]
destroyedNumbersParticleSystems keyTime shipId dir nums = do
  let laserSpeed = speed2vec $ coordsForDirection dir
  ps <- mapM (destroyedNumberParticleSystems keyTime shipId laserSpeed) $ Map.elems nums
  return $ List.concat ps

destroyedNumberParticleSystems :: (MonadState (AppState HamazedGame) m)
                               => Time Point ParticleSyst
                               -> ShipId
                               -> Vec2 Vel
                               -> Number
                               -> m [Prioritized ParticleSystem]
destroyedNumberParticleSystems k shipId laserSpeed (Number (NumberEssence (PosSpeed pos _) n _) _ _) = getPlayer shipId >>= maybe
  (return [])
  (\(Player _ _ (PlayerColors _ cycles)) -> do
    envFuncs <- envFunctions (WorldScope Air)
    let color i = cycleColors sumFrameParticleIndex $
                    if even i
                      then outer1 cycles
                      else wall2 cycles
        color' = cycleColors sumFrameParticleIndex $ wall2 cycles
    return
      $ map (Prioritized particleSystDefaultPriority)
      $ catMaybes [expandShrinkPolygon n pos color' (Speed 1) envFuncs k]
       ++ fragmentsFreeFallThenExplode (scalarProd 0.8 laserSpeed) pos color (gameGlyph $ intToDigit n) (Speed 2) envFuncs k)

outerSpaceParticleSystems :: (MonadState (AppState HamazedGame) m)
                          => Time Point ParticleSyst
                          -> ShipId
                          -> LaserRay Actual
                          -> m [Prioritized ParticleSystem]
outerSpaceParticleSystems t shipId ray@(LaserRay dir _ _) = getPlayer shipId >>= maybe
  (return [])
  (\(Player _ _ (PlayerColors _ cycles)) -> do
    world <- getWorld
    let space' = getWorldSpace world
        laserTarget = afterEnd ray
        glyph = materialGlyph Wall
    case location laserTarget space' of
          InsideWorld -> return []
          OutsideWorld ->
            if distanceToSpace laserTarget space' > 0
              then do
                let color _fragment _level _frame =
                      if 0 == _fragment `mod` 2
                        then
                          cycleColors sumFrameParticleIndex (outer1 cycles) $ quot _frame 4
                        else
                          cycleColors sumFrameParticleIndex (outer2 cycles) $ quot _frame 4
                    pos = translateInDir dir laserTarget
                    (speedAttenuation, nRebounds) = (0.3, 3)
                mode <- getViewMode
                screen <- getCurScreen
                case scopedLocation world mode screen NegativeWorldContainer pos of
                    InsideWorld -> outerSpaceParticleSystems' NegativeWorldContainer pos
                                    dir speedAttenuation nRebounds color glyph t
                    OutsideWorld -> return []
              else do
                let color _fragment _level _frame =
                      if 0 == _fragment `mod` 3
                        then
                          cycleColors sumFrameParticleIndex (wall1 cycles) $ quot _frame 4
                        else
                          cycleColors sumFrameParticleIndex (wall2 cycles) $ quot _frame 4
                    (speedAttenuation, nRebounds) = (0.4, 5)
                outerSpaceParticleSystems' (WorldScope Wall) laserTarget
                     dir speedAttenuation nRebounds color glyph t)

outerSpaceParticleSystems' :: (MonadState (AppState HamazedGame) m)
                           => Scope
                           -> Coords Pos
                           -> Direction
                           -> Float
                           -> Int
                           -> (Int -> Int -> Colorization)
                           -> Glyph
                           -> Time Point ParticleSyst
                           -> m [Prioritized ParticleSystem]
outerSpaceParticleSystems' scope afterLaserEndPoint dir speedAttenuation nRebounds colorFuncs glyph t = do
  envFuncs <- envFunctions scope
  let speed = scalarProd 0.8 $ speed2vec $ coordsForDirection dir
  return
    $ map (Prioritized particleSystDefaultPriority)
    $ fragmentsFreeFallWithReboundsThenExplode
      speed afterLaserEndPoint speedAttenuation nRebounds colorFuncs glyph
      (Speed 1) envFuncs t

laserParticleSystems :: (MonadState (AppState HamazedGame) m)
                     => Time Point ParticleSyst
                     -> ShipId
                     -> LaserRay Actual
                     -> m [Prioritized ParticleSystem]
laserParticleSystems t shipId ray = getPlayer shipId >>= maybe
  (return [])
  (\(Player _ _ (PlayerColors _ cycles)) ->
    return $ catMaybes
      [Prioritized particleSystLaserPriority <$> laserShot ray (cycleColors onlyFrame $ laser cycles) t])

checkTargetAndAmmo :: Int
                   -- ^ Remaining ammo
                   -> Int
                   -- ^ The current sum of all shot 'Numbers'
                   -> LevelTarget
                   -- ^ The 'Level' 's target number.
                   -> Maybe LevelOutcome
checkTargetAndAmmo ammo currentNumber (LevelTarget goal constraint) =
    checkSum <|> checkAmmo <|> Nothing
  where
    checkSum = case compare currentNumber goal of
      EQ -> Just Won
      LT -> Nothing
      GT -> case constraint of
        CanOvershoot -> Nothing
        CannotOvershoot -> Just $ Lost $ pack $ show currentNumber ++ " is bigger than " ++ show goal
    checkAmmo
      | ammo <= 0 = Just $ Lost $ pack "no ammo left"
      | otherwise = Nothing

checkComponentStatus :: (MonadState (AppState HamazedGame) m)
                     => ComponentIdx
                     -> m ()
checkComponentStatus i = countComponentAmmo i >>= \case
    0 ->
      getWorld >>= \w ->
        putWorld $ w {
          getWorldNumbers =
            Map.map
              (\n -> if i == getNumberCC (getNumEssence n)
                then
                  makeUnreachable n
                else
                  n
              ) $ getWorldNumbers w }
    _ ->
      return ()

checkAllComponentStatus :: (MonadState (AppState HamazedGame) m)
                        => m ()
checkAllComponentStatus = countComponentsAmmo >>= \ammos ->
  getWorld >>= \w -> do
    let nums = Map.map (\n -> case Map.findWithDefault 0 (getNumberCC $ getNumEssence n) ammos of
          0 -> makeUnreachable n
          _ -> n) $ getWorldNumbers w
    putWorld $ w { getWorldNumbers = nums }


countComponentAmmo :: (MonadState (AppState HamazedGame) m)
                     => ComponentIdx
                     -> m Int
countComponentAmmo i =
  Map.foldl'
    (\m ship@(BattleShip _ _ _ _ idx) ->
        if idx == i
          then
            m + countLiveAmmo ship
          else
            m)
    0 . getWorldShips <$> getWorld


countComponentsAmmo :: (MonadState (AppState HamazedGame) m)
                    => m (Map ComponentIdx Int)
countComponentsAmmo =
  Map.foldl'
    (\m ship@(BattleShip _ _ _ _ idx) ->
      let ammo = countLiveAmmo ship
          f Nothing = Just ammo
          f (Just x) = Just $ ammo + x
      in Map.alter f idx m)
    Map.empty . getWorldShips <$> getWorld

countAmmo :: [BattleShip] -> Int
countAmmo =
  List.foldl' (\s ship -> s + countLiveAmmo ship) 0

countLiveAmmo :: BattleShip -> Int
countLiveAmmo (BattleShip _ ammo status _ _) =
  if shipIsAlive status
    then
      ammo
    else
      0

-- | Discard sums that don't match the live ammo per connex components.
-- If a reachable number is in no sum, draw it in red.
checkSums :: (MonadState (AppState HamazedGame) m)
          => m ()
checkSums = getGameState >>= \(HamazedGame w@(World remainingNumbers _ _ _ _) _ shotNumbers
                                         (Level (LevelEssence _ (LevelTarget totalQty constraint) _) _) _ _) ->
  case constraint of
    CanOvershoot -> return ()
    CannotOvershoot -> do
      let shotQty = applyOperations $ reverse shotNumbers
          remainingQty = totalQty - shotQty
          remainingNums = map (getNumber . getNumEssence) $ Map.elems remainingNumbers

          asSet = ISet.fromList remainingNums
          distinct = ISet.size asSet == length remainingNums
          -- list of descending lists.
          allLists = bool
            (mkSumsN remainingNums remainingQty)
            -- there is a 4x performance benefit in using 'mkSumsStrict' when numbers are unique.
            (Tree.toList $ mkSumsStrict asSet remainingQty)
            distinct
          -- TODO group numbers by cc, then check if cc has enough ammo.
          -- (take into account that in case of multiple identical number,
          -- the same numbers could be in multiple components)
          possibleLists = allLists

          -- ISet.unions has a linear complexity, overall we have a linear complexity here.
          possibleSets = bool
            (map ISet.fromAscList)
            (map ISet.fromDistinctAscList)
            distinct $ reverse possibleLists

          okTargets = ISet.unions possibleSets

          -- preferred numbers are on the preferred path (least length)
          mapLengths = Map.fromListWith ISet.union (map (\l -> (ISet.size l,l)) possibleSets)
          preferredSet = maybe ISet.empty snd $ Map.lookupMin mapLengths

          newNumbers =
            Map.map
              (\n -> let num = getNumber $ getNumEssence n
                in if ISet.member num okTargets
                  then
                    if ISet.member num preferredSet
                      then
                        makePreferred n
                      else
                        makeReachable n
                  else
                    makeDangerous n)
            remainingNumbers
      putWorld $ w { getWorldNumbers = newNumbers }

applyOperations :: [ShotNumber] -> Int
applyOperations =
  List.foldl' (\v (ShotNumber n op) ->
            case op of
              Add -> v + n
              Substract -> v - n) 0

{-# INLINABLE getLevelOutcome #-}
getLevelOutcome :: MonadState (AppState HamazedGame) m => m (Maybe LevelOutcome)
getLevelOutcome = getLevelOutcome' <$> getLevel

{-# INLINABLE putLevelOutcome #-}
putLevelOutcome :: MonadState (AppState HamazedGame) m => Maybe LevelOutcome -> m ()
putLevelOutcome l = getLevel >>= \s -> putLevel s { getLevelOutcome' = l }

{-# INLINABLE getLevel #-}
getLevel :: MonadState (AppState HamazedGame) m => m Level
getLevel = getGameLevel <$> getGameState

{-# INLINABLE putLevel #-}
putLevel :: MonadState (AppState HamazedGame) m => Level -> m ()
putLevel l = getGameState >>= \s -> putGameState s { getGameLevel = l }

{-# INLINABLE getViewMode #-}
getViewMode :: MonadState (AppState HamazedGame) m => m ViewMode
getViewMode = getViewMode' <$> getGameState

{-# INLINABLE putAnimation #-}
putAnimation :: MonadState (AppState HamazedGame) m => UIAnimation -> m ()
putAnimation a =
  getGameState >>= \s -> putGameState $ s {getUIAnimation = a}

{-# INLINABLE putViewMode #-}
putViewMode :: MonadState (AppState HamazedGame) m => ViewMode -> m ()
putViewMode p =
  getGameState >>= \g -> putGameState $ g {getViewMode' = p}

{-# INLINABLE putWorld #-}
putWorld :: MonadState (AppState HamazedGame) m => World -> m ()
putWorld w = getGameState >>= \g -> putGameState g {currentWorld = w}

{-# INLINABLE getWorld #-}
getWorld :: MonadState (AppState HamazedGame) m => m World
getWorld = currentWorld <$> getGameState

{-# INLINABLE addParticleSystems #-}
addParticleSystems :: MonadState (AppState HamazedGame) m
                   => [Prioritized ParticleSystem]
                   -> m ()
addParticleSystems l = do
  keys <- takeKeys $ length l
  let ps2 = Map.fromDistinctAscList $ zip keys l
  gets game >>= \g -> putGame $ g {gameParticleSystems = Map.union (gameParticleSystems g) ps2}

-- | Creates environment functions taking into account a 'World' and 'Scope'
{-# INLINABLE envFunctions #-}
envFunctions :: (MonadState (AppState HamazedGame) m)
             => Scope -> m EnvFunctions
envFunctions scope = do
  world <- getWorld
  mode <- getViewMode
  screen <- getCurScreen
  return $ EnvFunctions (environmentInteraction world mode screen scope) envDistance
