{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Game.Hamazed.Loop.Update
      ( updateAppState
      , sendToServer
      ) where

import           Imj.Prelude

import           Control.Exception.Base(throwIO)
import           Control.Monad.Reader.Class(MonadReader, asks)

import           Data.Map.Strict(elems)
import           Data.Text(pack)
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
  EndLevel outcome -> do
    sendToServer $ ExitedState $ PlayLevel Running
    sendToServer $ LevelEnded outcome
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
  ConnectionAccepted name -> do
    putGameConnection $ Connected name
    sendToServer $ ExitedState Excluded
  ConnectionRefused reason ->
    putGameConnection $ ConnectionFailed reason
  ListPlayers players ->
    stateChat $ addMessage $ ChatMessage $ welcome players
  PlayerInfo (ClientId player _) notif ->
    stateChat $ addMessage $ ChatMessage $ toTxt notif player
  GameInfo notif ->
    stateChat $ addMessage $ ChatMessage $ toTxt' notif
  EnterState s -> putClientState $ ClientState Ongoing s
  ExitState s  -> putClientState $ ClientState Done s
  Disconnected reason -> onDisconnection reason
  Error txt ->
    liftIO $ throwIO $ ErrorFromServer txt
 where
  onDisconnection ClientShutdown     = liftIO $ throwIO GracefulClientEnd
  onDisconnection s@(BrokenClient _)   = liftIO $ throwIO $ UnexpectedProgramEnd $ "Broken Client : " <> pack (show s)
  onDisconnection s@(ServerShutdown _) = liftIO $ throwIO $ UnexpectedProgramEnd $ "Disconnected by Server: " <> pack (show s)


{-# INLINABLE sendToServer #-}
sendToServer :: (MonadState AppState m, MonadIO m, MonadReader e m, ClientNode e)
             => ClientEvent
             -> m ()
sendToServer e = do
  case e of
    ExitedState state -> do
      getClientState >>= \cur ->
        when (cur /= ClientState Ongoing state) $
          error $ "ExitedState " ++ show state ++ " in " ++ show cur  -- sanity check
      putClientState $ ClientState Done state
    _ -> return ()
  asks sendToServer' >>= \f -> f e

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
  getGameState >>= \(GameState b c d (Level n mayFinished) e f) ->
    case mayFinished of
      Just (LevelFinished stop finishTime _) -> do
        let newLevel = Level n (Just $ LevelFinished stop finishTime ContinueMessage)
        putGameState $ GameState b c d newLevel e f
      Nothing -> return ()

{-# INLINABLE onLaser #-}
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
  getGameState >>= \(GameState w@(World _ ships _ _ _ _) f g (Level level@(LevelSpec _ target _) finished) a s) -> do
    let allShotNumbers = g ++ map (\(Number _ n) -> n) destroyedBalls
        newLevel = Level level $ finished <|> checkTargetAndAmmo (countAmmo $ elems ships) (sum allShotNumbers) target t
    putGameState $ GameState w f allShotNumbers newLevel a s
    updateShipsText

{-# INLINABLE updateShipsText #-}
updateShipsText :: (MonadState AppState m)
                => m ()
updateShipsText =
  getGameState >>= \(GameState (World _ ships space _ _ _) _ shotNumbers (Level level _)
                               (UIAnimation (UIEvolutions j upDown _) p) _) -> do
    mode <- getViewMode
    (Screen _ center) <- getCurScreen
    let newLeft =
          let frameSpace = mkRectContainerWithCenterAndInnerSize center $ getSize space
              (horizontalDist, verticalDist) = computeViewDistances mode
              (_, _, leftMiddle, _) = getSideCenters $ mkRectContainerAtDistance frameSpace horizontalDist verticalDist
              infos = mkLeftInfo Normal (elems ships) shotNumbers level
          in mkTextAnimRightAligned leftMiddle leftMiddle infos 1 (fromSecs 1)
        newAnim = UIAnimation (UIEvolutions j upDown newLeft) p -- TODO use mkUIAnimation to have a smooth transition
    putAnimation $ assert (isFinished newAnim) newAnim


{-# INLINABLE onMove #-}
onMove :: (MonadState AppState m, MonadIO m)
       => [(ShipId, Coords Vel)]
       -> [ShipId]
       -> m ()
onMove accelerations shipsLosingArmor = do
  getWorld >>= putWorld . moveWorld accelerations shipsLosingArmor
  onHasMoved

{-# INLINABLE onHasMoved #-}
onHasMoved :: (MonadState AppState m, MonadIO m)
           => m ()
onHasMoved =
  liftIO getSystemTime >>= \t -> shipParticleSystems t >>= addParticleSystems >> getGameState
    >>= \(GameState world@(World balls ships _ _ _ _) f shotNums (Level level@(LevelSpec _ target _) finished) anim s) -> do
      let oneShipAlive = any (shipIsAlive . getShipStatus) ships
          allCollisions =
            concatMap
            (\(BattleShip _ _ _ shipStatus collisions) ->
              case shipStatus of
                Armored -> []
                _ -> collisions)
            ships
          remainingBalls = filter (`notElem` allCollisions) balls
          newWorld = world { getWorldNumbers = remainingBalls }
          finishIfAllShipsDestroyed =
            if oneShipAlive
              then Nothing
              else
                case allCollisions of
                  [] -> Nothing
                  _  ->
                    let msg = "collision with " <> showListOrSingleton (map getNumber allCollisions)
                    in Just $ LevelFinished (Lost msg) t InfoMessage
          finishIfNoAmmo = checkTargetAndAmmo (countAmmo $ elems ships) (sum shotNums) target t
          newLevel = Level level (finished <|> finishIfAllShipsDestroyed <|> finishIfNoAmmo)
      putGameState $ assert (isFinished anim) $ GameState newWorld f shotNums newLevel anim s
      updateShipsText

{-# INLINABLE updateUIAnim #-}
updateUIAnim :: (MonadState AppState m, MonadIO m, MonadReader e m, ClientNode e)
             => Time Point System -> m ()
updateUIAnim t =
  getGameState >>= \(GameState curWorld futWorld j k a@(UIAnimation evolutions (UIAnimProgress _ it)) s) -> do
    let nextIt@(Iteration _ nextFrame) = nextIteration it
        (world, worldAnimDeadline) =
          maybe
            (futWorld, Nothing) -- to copy the future world to the current world
            (\dt -> (curWorld, Just $ addDuration dt t))
            $ getDeltaTime evolutions nextFrame
        anims = a { getProgress = UIAnimProgress worldAnimDeadline nextIt }
    putGameState $ GameState world futWorld j k anims s
    when (isFinished anims) $
      maybe (return ()) (sendToServer . IsReady) $ getId world
