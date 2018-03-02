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

import           Data.Attoparsec.Text(parseOnly)
import qualified Data.Map.Strict as Map (elems, map)
import           Data.Text(pack, strip)
import           System.Exit(exitSuccess)
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.State.Types
import           Imj.Game.Hamazed.Types

import           Imj.Game.Hamazed.Command
import           Imj.Game.Hamazed.Loop.Create
import           Imj.Game.Hamazed.Loop.Event
import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Game.Hamazed.Network.Class.ClientNode
import           Imj.Game.Hamazed.World
import           Imj.Game.Hamazed.World.Create
import           Imj.Game.Hamazed.World.Ship
import           Imj.Graphics.Render.FromMonadReader
import           Imj.Graphics.Text.ColorString
import           Imj.Util

{-# INLINABLE updateAppState #-}
updateAppState :: (MonadState AppState m, MonadReader e m, Render e, ClientNode e, MonadIO m)
               => UpdateEvent
               -- ^ The 'Event' that should be handled here.
               -> m ()
updateAppState (Right evt) = case evt of
  (Interrupt Quit) -> sendToServer $ RequestCommand $ Leaves Intentional
  (Interrupt Help) -> error "not implemented"
  Log txt -> stateChat $ addMessage $ Warning txt
  Configuration c ->
    updateGameParamsFromChar c
  CycleRenderingOptions ->
    cycleRenderingOptions
  EndLevel outcome -> do
    sendToServer $ ExitedState $ PlayLevel Running
    sendToServer $ LevelEnded outcome
  (Timeout (Deadline t _ AnimateUI)) -> updateUIAnim t
  (Timeout (Deadline _ _ (AnimateParticleSystem key))) -> liftIO getSystemTime >>= updateOneParticleSystem key
  (Timeout (Deadline _ _ DisplayContinueMessage)) -> onContinueMessage
  ChatCmd chatCmd -> stateChat $ flip (,) () . runChat chatCmd
  SendChatMessage -> onSendChatMessage
  ToggleEventRecording -> error "should be handled by caller"
updateAppState (Left evt) = case evt of
  RunCommand i cmd -> runClientCommand i cmd
  CommandError cmd err ->
    stateChat $ addMessage $ Warning $
      pack (show cmd) <> " failed:" <> err
  Reporting cmd res ->
    stateChat $ addMessage $ Info $
      pack (show cmd) <> " is:" <> res
  WorldRequest spec ->
    liftIO (mkWorldEssence spec) >>= sendToServer . WorldProposal
  CurrentGameStateRequest ->
    sendToServer . CurrentGameState . mkGameStateEssence =<< getGameState
  ChangeLevel levelEssence worldEssence ->
    getGameState >>= \state@(GameState _ _ _ _ _ (Screen sz _) viewMode names) ->
      mkInitialState levelEssence worldEssence names viewMode sz (Just state)
        >>= putGameState
  PutGameState (GameStateEssence worldEssence shotNums levelEssence) ->
    getGameState >>= \state@(GameState _ _ _ _ _ (Screen sz _) viewMode names) ->
      mkIntermediateState shotNums levelEssence worldEssence names viewMode sz (Just state)
        >>= putGameState
  GameEvent (PeriodicMotion accelerations shipsLosingArmor) ->
    onMove accelerations shipsLosingArmor
  GameEvent (LaserShot dir shipId) ->
    onLaser shipId dir Add
  ConnectionAccepted i eplayers -> do
    sendToServer $ ExitedState Excluded
    putGameConnection $ Connected i
    let p = Map.map mkPlayer eplayers
    putPlayers p
    stateChat $ addMessage $ ChatMessage $ welcome p
  ConnectionRefused reason ->
    putGameConnection $ ConnectionFailed reason
  PlayerInfo notif i ->
    stateChat . addMessage . ChatMessage =<< toTxt i notif
  GameInfo notif ->
    stateChat $ addMessage $ ChatMessage $ toTxt' notif
  EnterState s -> putClientState $ ClientState Ongoing s
  ExitState s  -> putClientState $ ClientState Over s
  Disconnected reason -> onDisconnection reason
  Error txt ->
    liftIO $ throwIO $ ErrorFromServer txt
 where
  onDisconnection ClientShutdown       = liftIO $ exitSuccess
  onDisconnection s@(BrokenClient _)   = liftIO $ throwIO $ UnexpectedProgramEnd $ "Broken Client : " <> pack (show s)
  onDisconnection s@(ServerShutdown _) = liftIO $ throwIO $ UnexpectedProgramEnd $ "Disconnected by Server: " <> pack (show s)

  toTxt i notif =
    (`mappend` colored (toTxt'' notif) chatMsgColor) . getPlayerUIName' <$> getPlayer i
  toTxt'' = \case
    Joins        -> " joins the game."
    WaitsToJoin  -> " is waiting to join the game."
    StartsGame   -> " starts the game."
    Done cmd res -> " initiated " <> pack (show cmd) <> " resulting in:" <> res

  toTxt' (LevelResult n (Lost reason)) =
    colored ("- Level " <> pack (show n) <> " was lost : " <> reason <> ".") chatMsgColor
  toTxt' (LevelResult n Won) =
    colored ("- Level " <> pack (show n) <> " was won!") chatWinColor
  toTxt' GameWon =
    colored "- The game was won! Congratulations!" chatWinColor

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
      putClientState $ ClientState Over state
    _ -> return ()
  asks sendToServer' >>= \f -> f e

onSendChatMessage :: (MonadState AppState m, MonadIO m, MonadReader e m, ClientNode e)
                  => m ()
onSendChatMessage =
  strip <$> stateChat takeMessage >>= \msg -> do
    let left = stateChat . addMessage . Warning . (<>) ("Error while parsing: " <> msg <> " : ")
        p = parseOnly command msg
    either
      (left . pack)
      (either
        left
        (\case
            ServerRep rep -> sendToServer $ Report rep
            ServerCmd cmd -> sendToServer $ Do cmd
            ClientCmd cmd -> sendToServer $ RequestCommand cmd))
      p

updateGameParamsFromChar :: (MonadState AppState m, MonadIO m, MonadReader e m, ClientNode e)
                         => Char
                         -> m ()
updateGameParamsFromChar = \case
  '1' -> sendToServer $ ChangeWorldShape Square
  '2' -> sendToServer $ ChangeWorldShape Rectangle2x1
  'e' -> sendToServer $ ChangeWallDistribution None
  'r' -> sendToServer $ ChangeWallDistribution $ Random $ RandomParameters minRandomBlockSize OneComponentPerShip
  'd' -> putViewMode CenterSpace -- TODO force a redraw?
  'f' -> getMyShipId >>= maybe (return ()) (putViewMode . CenterShip)  -- TODO force a redraw?
  _ -> return ()

onContinueMessage :: (MonadState AppState m)
                  => m ()
onContinueMessage =
  getLevelStatus >>= maybe
    (return ())
    (\(LevelFinished stop finishTime _) ->
        putLevelStatus $ Just $ LevelFinished stop finishTime ContinueMessage)

{-# INLINABLE onLaser #-}
onLaser :: (MonadState AppState m, MonadIO m)
        => ShipId
        -> Direction
        -> Operation
        -> m ()
onLaser ship dir op =
  liftIO getSystemTime >>= \t ->
    laserEventAction ship dir t >>= onDestroyedNumbers t op

{-# INLINABLE onDestroyedNumbers #-}
onDestroyedNumbers :: (MonadState AppState m)
                   => Time Point System
                   -> Operation
                   -> [Number]
                   -> m ()
onDestroyedNumbers t op destroyedBalls =
  getGameState >>= \(GameState w@(World _ ships _ _ _ _) f g (Level level@(LevelEssence _ target _) finished) a s m na) -> do
    let allShotNumbers = g ++ map (\(Number _ n) -> ShotNumber n op) destroyedBalls
        newLevel = Level level $ finished <|> checkTargetAndAmmo (countAmmo $ Map.elems ships) (applyOperations $ reverse allShotNumbers) target t
    putGameState $ GameState w f allShotNumbers newLevel a s m na

{-# INLINABLE onMove #-}
onMove :: (MonadState AppState m, MonadIO m)
       => Map ShipId (Coords Vel)
       -> Set ShipId
       -> m ()
onMove accelerations shipsLosingArmor = do
  getWorld >>= putWorld . moveWorld accelerations shipsLosingArmor
  onHasMoved

{-# INLINABLE onHasMoved #-}
onHasMoved :: (MonadState AppState m, MonadIO m)
           => m ()
onHasMoved =
  liftIO getSystemTime >>= \t -> shipParticleSystems t >>= addParticleSystems >> getGameState
    >>= \(GameState world@(World balls ships _ _ _ _) f shotNums (Level level@(LevelEssence _ target _) finished) anim s m n) -> do
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
          finishIfNoAmmo = checkTargetAndAmmo (countAmmo $ Map.elems ships) (applyOperations $ reverse shotNums) target t
          newLevel = Level level (finished <|> finishIfAllShipsDestroyed <|> finishIfNoAmmo)
      putGameState $ assert (isFinished anim) $ GameState newWorld f shotNums newLevel anim s m n
      updateShipsText

{-# INLINABLE updateUIAnim #-}
updateUIAnim :: (MonadState AppState m, MonadIO m, MonadReader e m, ClientNode e)
             => Time Point System -> m ()
updateUIAnim t =
  getGameState >>= \(GameState curWorld mayFutWorld j k a@(UIAnimation evolutions (UIAnimProgress _ it)) s m names) -> do
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
    putGameState $ GameState world futWorld j k anims s m names
    when (isFinished anims) $
      maybe (return ()) (sendToServer . IsReady) $ getId world
