{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Game.Hamazed.Loop.Update
      ( updateAppState
      ) where

import           Imj.Prelude

import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.State.Types
import           Imj.Game.Hamazed.Types

import           Imj.Game.Hamazed.Chat
import           Imj.Game.Hamazed.Infos
import           Imj.Game.Hamazed.Loop.Create
import           Imj.Game.Hamazed.Loop.Event
import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Game.Hamazed.Network.Class.ClientNode
import           Imj.Game.Hamazed.World
import           Imj.Game.Hamazed.World.Create
import           Imj.Game.Hamazed.World.Ship
import           Imj.Graphics.Render.FromMonadReader
import           Imj.Graphics.UI.RectContainer
import           Imj.Util

{-# INLINABLE updateAppState #-}
updateAppState :: (MonadState AppState m, MonadReader e m, Draw e, ClientNode e, MonadIO m)
               => UpdateEvent
               -- ^ The 'Event' that should be handled here.
               -> m ()
updateAppState (Right evt) = case evt of
  (Interrupt Quit) -> sendToServer Disconnect
  (Interrupt Help) -> error "not implemented"
  Configuration char ->
    updateGameParamsFromChar char
  CycleRenderingOptions ->
    changeFont
  StartGame -> do
    getClientState >>= \case -- sanity check
      (ClientState Ongoing Setup) -> return ()
      s -> error $ "StartGame in " ++ show s
    putClientState $ ClientState Done Setup
    sendToServer $ ExitedState Setup
  NextLevel -> do
    getClientState >>= \case -- sanity check
      (ClientState Ongoing PlayLevel) -> return ()
      s -> error $ "NextLevel in " ++ show s
    putClientState $ ClientState Done PlayLevel
    sendToServer $ ExitedState PlayLevel -- TODO is it really necessary ? (are ExitedState events used by the server?)
    sendToServer LevelWon
  EndGame outcome ->
    sendToServer $ GameEnded outcome
  (Timeout (Deadline t _ AnimateUI)) -> updateUIAnim t
  (Timeout (Deadline _ _ (AnimateParticleSystem key))) -> liftIO getSystemTime >>= updateOneParticleSystem key
  (Timeout (Deadline _ _ DisplayContinueMessage)) -> onContinueMessage
  ToggleEventRecording -> error "should be handled by caller"
updateAppState (Left evt) = case evt of
  WorldRequest spec ->
    liftIO (mkWorldEssence spec) >>= sendToServer . WorldProposal
  ChangeLevel levelSpec worldEssence ->
    getGame >>= \(Game _ viewMode state@(GameState _ _ _ _ _ (Screen sz _)) _ _ _ _) ->
      mkInitialState levelSpec worldEssence viewMode sz (Just state)
        >>= putGameState
  GameEvent (PeriodicMotion accelerations shipsLosingArmor) ->
    onMove accelerations shipsLosingArmor
  GameEvent (LaserShot shipId dir) ->
    onLaser shipId dir
  ConnectionAccepted name players -> do
    putGameConnection $ Connected name
    stateChat (addMessage $ ChatMessage $ welcome players)
    getClientState >>= \case -- sanity check
      (ClientState Ongoing Excluded) -> return ()
      s -> error $ "ConnectionAccepted in " ++ show s
    putClientState $ ClientState Done Excluded
    sendToServer $ ExitedState Excluded -- TODO is it really necessary ? (are ExitedState events used by the server?)
  ConnectionRefused reason ->
    putGameConnection $ ConnectionFailed reason
  PlayerInfo (ClientId player _) notif ->
    stateChat $ addMessage $ ChatMessage $ toTxt notif player
  GameInfo notif ->
    stateChat $ addMessage $ ChatMessage $ toTxt' notif
  DisconnectionAccepted -> error "should be handled by caller"
  EnterState s -> putClientState $ ClientState Ongoing s
  ExitState s -> putClientState $ ClientState Done s
  Error txt -> error $ "[from Server:] " ++ txt

updateGameParamsFromChar :: (MonadState AppState m, MonadIO m, MonadReader e m, ClientNode e)
                         => Char
                         -> m ()
updateGameParamsFromChar = \case
  '1' -> sendToServer $ ChangeWorldShape Square
  '2' -> sendToServer $ ChangeWorldShape Rectangle2x1
  'e' -> sendToServer $ ChangeWallDistribution None
  'r' -> sendToServer $ ChangeWallDistribution Deterministic
  't' -> sendToServer $ ChangeWallDistribution $ Random $ RandomParameters minRandomBlockSize StrictlyOneComponent
  'd' -> putViewMode CenterSpace -- TODO force a redraw?
  'f' -> getMyShipId >>= maybe (return ()) (putViewMode . CenterShip)  -- TODO force a redraw?
  _ -> return ()

onContinueMessage :: (MonadState AppState m)
                  => m ()
onContinueMessage =
  getGameState >>= \(GameState b c d (Level n mayFinished) e f) -> do
    case mayFinished of
      Just (LevelFinished stop finishTime _) -> do
        let newLevel = Level n (Just $ LevelFinished stop finishTime ContinueMessage)
        putGameState $ GameState b c d newLevel e f
      Nothing -> return ()

onLaser :: (MonadState AppState m, MonadIO m)
        => ShipId
        -> Direction
        -> m ()
onLaser ship dir =
  liftIO getSystemTime >>= \t ->
    laserEventAction ship dir t >>= onDestroyedNumbers t

{-# INLINABLE onDestroyedNumbers #-}
onDestroyedNumbers :: (MonadState AppState m)
                   => Time Point System
                   -> [Number]
                   -> m ()
onDestroyedNumbers t destroyedBalls =
  getGameState >>= \(GameState world@(World _ ships space _ _ _)
                               futureWorld g (Level level@(LevelSpec _ target _) finished)
                   (UIAnimation (UIEvolutions j upDown _) k l) s) -> do
    (Screen _ center) <- getCurScreen
    mode <- getViewMode
    let destroyedNumbers = map (\(Number _ n) -> n) destroyedBalls
        allShotNumbers = g ++ destroyedNumbers
        ammos = map getAmmo ships
        allAmmos = sum ammos
        newLeft =
          let frameSpace = mkRectContainerWithCenterAndInnerSize center $ getSize space
              (horizontalDist, verticalDist) = computeViewDistances mode
              (_, _, leftMiddle, _) = getSideCenters $ mkRectContainerAtDistance frameSpace horizontalDist verticalDist
              infos = mkLeftInfo Normal ammos allShotNumbers level
          in mkTextAnimRightAligned leftMiddle leftMiddle infos 1 (fromSecs 0) -- 0 duration, since animation is over anyway
        newFinished = finished <|> checkTargetAndAmmo allAmmos (sum allShotNumbers) target t
        newLevel = Level level newFinished
        newAnim = UIAnimation (UIEvolutions j upDown newLeft) k l
    putGameState
      $ assert (isFinished newAnim)
      $ GameState world futureWorld allShotNumbers newLevel newAnim s

{-# INLINABLE onMove #-}
onMove :: (MonadState AppState m, MonadIO m)
       => [(ShipId, Coords Vel)]
       -> [ShipId]
       -> m ()
onMove accelerations shipsLosingArmor =
  getWorld >>= putWorld . moveWorld accelerations shipsLosingArmor
    >> onHasMoved

{-# INLINABLE onHasMoved #-}
onHasMoved :: (MonadState AppState m, MonadIO m)
           => m ()
onHasMoved =
  liftIO getSystemTime >>= \t -> shipParticleSystems t >>= addParticleSystems >> getGameState
    >>= \(GameState world@(World balls ships _ _ _ _) futureWorld shotNums (Level lt finished) anim s) -> do
    let allCollisions =
          concatMap
          (\(BattleShip _ _ _ safe collisions) ->
            if safe
              then []
              else collisions)
          ships
        remainingBalls = filter (`notElem` allCollisions) balls
        newWorld = world { getWorldNumbers = remainingBalls }
        finishIfOneShipCollides =
          case allCollisions of
            [] -> Nothing
            _  ->
              let msg = "collision with " <> showListOrSingleton (map getNumber allCollisions)
              in Just $ LevelFinished (Lost msg) t InfoMessage
        newLevel = Level lt (finished <|> finishIfOneShipCollides)
    putGameState $ assert (isFinished anim) $ GameState newWorld futureWorld shotNums newLevel anim s

{-# INLINABLE updateUIAnim #-}
updateUIAnim :: (MonadState AppState m, MonadIO m, MonadReader e m, ClientNode e)
             => Time Point System -> m ()
updateUIAnim t =
  getGameState >>= \(GameState curWorld futWorld j k (UIAnimation evolutions _ it) s) -> do
    let nextIt@(Iteration _ nextFrame) = nextIteration it
        (world, worldAnimDeadline) =
          maybe
            (futWorld, Nothing)
            (\dt ->
             (curWorld, Just $ addDuration dt t))
            $ getDeltaTime evolutions nextFrame
        anims = UIAnimation evolutions worldAnimDeadline nextIt
    putGameState $ GameState world futWorld j k anims s
    when (isFinished anims) $
      maybe (return ()) (sendToServer . IsReady) $ getId world
