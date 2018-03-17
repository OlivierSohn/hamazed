{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Game.Hamazed.Loop.Update
      ( updateAppState
      , sendToServer
      ) where

import           Imj.Prelude hiding(intercalate)

import           Control.Exception.Base(throwIO)
import           Control.Monad.Reader.Class(MonadReader, asks)
import           Control.Monad.Reader(runReaderT)

import           Data.Attoparsec.Text(parseOnly)
import           Data.List(foldl')
import qualified Data.Map.Strict as Map (lookup, filter, keysSet, size, elems, map, withoutKeys, restrictKeys)
import qualified Data.Set as Set (empty, union, size, null, toList)
import           Data.Text(pack, unpack, strip)
import           System.Exit(exitSuccess)
import           System.IO(putStrLn)

import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.State.Types
import           Imj.Game.Hamazed.Types

import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.Command
import           Imj.Game.Hamazed.Loop.Create
import           Imj.Game.Hamazed.Loop.Event
import           Imj.Game.Hamazed.Loop.Event.Priorities
import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Game.Hamazed.Network.Class.ClientNode
import           Imj.Game.Hamazed.World
import           Imj.Game.Hamazed.World.Create
import           Imj.Game.Hamazed.World.Ship
import           Imj.Graphics.Class.HasSizedFace
import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.RecordDraw
import           Imj.Graphics.Render.FromMonadReader
import           Imj.Graphics.Text.ColorString
import           Imj.Graphics.Text.RasterizedString
import           Imj.Graphics.UI.RectContainer
import           Imj.Util

{-# INLINABLE updateAppState #-}
updateAppState :: (MonadState AppState m
                 , MonadReader e m, ClientNode e, Render e, HasSizedFace e
                 , MonadIO m)
               => UpdateEvent
               -- ^ The 'Event' that should be handled here.
               -> m ()
updateAppState (Right evt) = case evt of
  Interrupt Quit -> sendToServer $ RequestCommand $ Leaves Intentional
  Interrupt Help -> error "not implemented"
  Continue x -> sendToServer $ CanContinue x
  Log Error txt -> error $ unpack txt
  Log msgLevel txt -> stateChat $ addMessage $ Information msgLevel txt
  Configuration c ->
    updateGameParamsFromChar c
  CycleRenderingOptions i j -> do
    cycleRenderingOptions i j >>= either (liftIO . putStrLn) return
    onTargetSize
  ApplyPPUDelta deltaPPU -> do
    -- the font calculation is done according to the current screen size,
    -- so there may be some partial unit rectangles. If we wanted to have only
    -- full unit rectangles, we could recompute the screen size based on preferred size and new ppu,
    -- then adjust the font.
    asks applyPPUDelta >>= \f -> f deltaPPU >>= either (liftIO . putStrLn) return
    onTargetSize
  ApplyFontMarginDelta d ->
    asks applyFontMarginDelta >>= \f -> f d >>= either (liftIO . putStrLn) return
  CanvasSizeChanged ->
    onTargetSize
  RenderingTargetChanged -> do
    onTargetChanged >>= either (liftIO . putStrLn) return
    onTargetSize
  Timeout (Deadline t _ AnimateUI) -> updateUIAnim t
  Timeout (Deadline _ _ (AnimateParticleSystem key)) -> liftIO getSystemTime >>= updateOneParticleSystem key
  Timeout (Deadline t _ (RedrawStatus f)) -> updateStatus (Just f) t
  ChatCmd chatCmd -> stateChat $ flip (,) () . runChat chatCmd
  SendChatMessage -> onSendChatMessage
  ToggleEventRecording -> error "should be handled by caller"
updateAppState (Left evt) = case evt of
  RunCommand i cmd -> runClientCommand i cmd
  CommandError cmd err ->
    stateChat $ addMessage $ Information Warning $
      pack (show cmd) <> " failed:" <> err
  Reporting cmd res ->
    stateChat $ addMessage $ Information Info $
      pack (show cmd) <> " is:" <> res
  WorldRequest spec ->
    liftIO (mkWorldEssence spec) >>= sendToServer . WorldProposal
  CurrentGameStateRequest ->
    sendToServer . CurrentGameState . mkGameStateEssence =<< getGameState
  ChangeLevel levelEssence worldEssence ->
    getGameState >>= \state@(GameState _ _ _ _ _ _ (Screen sz _) viewMode names) ->
      mkInitialState levelEssence worldEssence names viewMode sz (Just state)
        >>= putGameState
  PutGameState (GameStateEssence worldEssence shotNums levelEssence) ->
    getGameState >>= \state@(GameState _ _ _ _ _ _ (Screen sz _) viewMode names) ->
      mkIntermediateState shotNums levelEssence worldEssence names viewMode sz (Just state)
        >>= putGameState
  GameEvent (PeriodicMotion accelerations shipsLosingArmor) ->
    onMove accelerations shipsLosingArmor
  GameEvent (LaserShot dir shipId) ->
    onLaser shipId dir Add
  ConnectionAccepted i eplayers -> do
    sendToServer $ ExitedState Excluded
    putClientState $ ClientState Over Excluded
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
  EnterState s ->
    putClientState $ ClientState Ongoing s
  ExitState s  ->
    putClientState $ ClientState Over s
  Disconnected reason -> onDisconnection reason
  ServerError txt ->
    liftIO $ throwIO $ ErrorFromServer txt
 where
  onDisconnection ClientShutdown       = liftIO exitSuccess
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


{-# INLINABLE onTargetSize #-}
onTargetSize :: (MonadState AppState m
               , MonadReader e m, Canvas e
               , MonadIO m)
             => m ()
onTargetSize = getTargetSize >>= maybe (return ()) (\sz ->
  getGameState >>= \g@(GameState curWorld mayNewWorld _ _ uiAnimation _ _ _ _) -> do
    let screen@(Screen _ newScreenCenter) = mkScreen $ Just sz
        sizeSpace = getSize $ getWorldSpace $ fromMaybe curWorld mayNewWorld
        newPosition = upperLeftFromCenterAndSize newScreenCenter sizeSpace
        newAnim = setPosition newPosition uiAnimation
    putGameState $ g { getUIAnimation = newAnim,
                       getScreen = screen })

{-# INLINABLE sendToServer #-}
sendToServer :: (MonadState AppState m
               , MonadReader e m, ClientNode e
               , MonadIO m)
             => ClientEvent
             -> m ()
sendToServer e =
  asks sendToServer' >>= \f -> f e

onSendChatMessage :: (MonadState AppState m
                    , MonadReader e m, ClientNode e
                    , MonadIO m)
                  => m ()
onSendChatMessage =
  strip <$> stateChat takeMessage >>= \msg -> do
    let left = stateChat . addMessage . Information Warning . (<>) ("Error while parsing: " <> msg <> " : ")
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

updateGameParamsFromChar :: (MonadState AppState m
                           , MonadReader e m, ClientNode e
                           , MonadIO m)
                         => Char
                         -> m ()
updateGameParamsFromChar = \case
  '1' -> sendToServer $ ChangeWorldShape Square
  '2' -> sendToServer $ ChangeWorldShape Rectangle2x1
  'e' -> sendToServer $ ChangeWallDistribution None
  'r' -> sendToServer $ ChangeWallDistribution $ Random $ RandomParameters minRandomBlockSize OneComponentPerShip
  {-
  'd' -> putViewMode CenterSpace -- TODO force a redraw?
  'f' -> getMyShipId >>= maybe (return ()) (putViewMode . CenterShip)  -- TODO force a redraw?
  -}
  _ -> return ()

{-# INLINABLE onLaser #-}
onLaser :: (MonadState AppState m
          , MonadReader e m, ClientNode e
          , MonadIO m)
        => ShipId
        -> Direction
        -> Operation
        -> m ()
onLaser ship dir op =
  (liftIO getSystemTime >>= laserEventAction ship dir) >>= onDestroyedNumbers
 where
  onDestroyedNumbers (destroyedBalls, ammoChanged) =
    getGameState >>= \(GameState w@(World _ ships _ _ _ _) f g (Level level@(LevelEssence _ target _) finished) a s b m na) -> do
      let allShotNumbers = g ++ map (flip ShotNumber op . getNumber . getNumEssence) (Map.elems destroyedBalls)
          finishIfNoAmmo = checkTargetAndAmmo (countAmmo $ Map.elems ships) (applyOperations $ reverse allShotNumbers) target
          newFinished = finished <|> finishIfNoAmmo
          newLevel = Level level newFinished
      maybe
        (return ())
        (when (isNothing finished) . sendToServer . LevelEnded)
        newFinished
      putGameState $ GameState w f allShotNumbers newLevel a s b m na
      updateShipsText
      when ammoChanged checkSums


{-# INLINABLE onMove #-}
onMove :: (MonadState AppState m
         , MonadReader e m, ClientNode e
         , MonadIO m)
       => Map ShipId (Coords Vel)
       -> Set ShipId
       -> m ()
onMove accelerations shipsLosingArmor = do
  moveWorld accelerations shipsLosingArmor
  onHasMoved

{-# INLINABLE onHasMoved #-}
onHasMoved :: (MonadState AppState m
             , MonadReader e m, ClientNode e
             , MonadIO m)
           => m ()
onHasMoved =
  liftIO getSystemTime >>= shipParticleSystems >>= addParticleSystems >> getGameState
    >>= \(GameState world@(World balls ships _ _ _ _) f shotNums (Level level@(LevelEssence _ target _) finished) anim b o m n) -> do
      let oneShipAlive = any (shipIsAlive . getShipStatus) ships
          allCollisions =
            foldl'
            (\s (BattleShip _ _ _ shipStatus collisions _) ->
              case shipStatus of
                Armored -> s
                _ -> Set.union s collisions)
            Set.empty
            ships
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
      putGameState $ assert (isFinished anim) $ GameState newWorld f shotNums newLevel anim b o m n
      when numbersChanged checkSums
      updateShipsText

{-# INLINABLE updateUIAnim #-}
updateUIAnim :: (MonadState AppState m
               , MonadReader e m, ClientNode e
               , MonadIO m)
             => Time Point System -> m ()
updateUIAnim t =
  getGameState >>= \(GameState curWorld mayFutWorld j k a@(UIAnimation evolutions (UIAnimProgress _ it)) b s m names) -> do
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
    putGameState $ GameState world futWorld j k anims b s m names
    when (isFinished anims) $ do
      checkAllComponentStatus
      checkSums
      maybe (return ()) (sendToServer . IsReady) $ getId world

{-# INLINABLE putClientState #-}
putClientState :: (MonadState AppState m
                 , MonadReader e m, HasSizedFace e
                 , MonadIO m)
               => ClientState
               -> m ()
putClientState i = do
  gets game >>= \g -> putGame $ g { getClientState = i}
  liftIO getSystemTime >>= updateStatus Nothing

{-# INLINABLE updateStatus #-}
updateStatus :: (MonadState AppState m
               , MonadReader e m, HasSizedFace e
               , MonadIO m)
             => Maybe Frame
             -- ^ When Nothing, the current frame should be used.
             -> Time Point System
             -> m ()
updateStatus mayFrame t = gets game >>= \(Game state (GameState _ _ _ _ _ drawnState (Screen _ ref) _ _) _ _ _ _) -> do
  newStrs <- go state
  -- we could have a datastructure where each line has its corresponding 'RecordDraw'
  -- and
  let canInterrupt = maybe
        True
        (\(oldState,oldStrs,_) -> oldState /= state && newStrs /= oldStrs)
        drawnState
  when canInterrupt $ do
    let mayPrevRecord =
          maybe
            Nothing
            (\(_,_,(Evolution (Successive s) _ _ _, _, _)) ->
              case s of
                [] -> Nothing
                _ -> Just $ last s)
            drawnState
    recordFromStrs ref newStrs state >>= (\newRecord -> do
        evolutionStart <- flip fromMaybe mayPrevRecord <$> liftIO mkZeroRecordDraw
        return $ Just ( newStrs
                      , ( if null newStrs
                            then
                              mkEvolutionEaseInQuart (Successive [evolutionStart,newRecord]) $ fromSecs 0.5
                            else
                              mkEvolutionEaseQuart (Successive [evolutionStart,newRecord]) $ fromSecs 1
                        , 0
                        , Nothing)))
      >>= putDrawnState
  updateStatusDeadline
 where
  updateStatusDeadline :: MonadState AppState m
                       => m ()
  updateStatusDeadline =
    getDrawnClientState <$> getGameState >>= maybe
      (return ())
      (\(_,strs,(recordEvolution, curFrame, _)) -> do
        let frame = fromMaybe curFrame mayFrame
            minDt = fromSecs 0.015
            significantDeadline f sofar = maybe
              (succ f, sofar)
              (\dt ->
                let newDuration = dt |+| fromMaybe (fromSecs 0) sofar
                in if newDuration > minDt
                    then
                      (succ f, Just newDuration)
                    else
                      significantDeadline (succ f) $ Just newDuration)
              $ getDeltaTimeToNextFrame recordEvolution f
            (deadlineFrame, deadlineGap) = significantDeadline frame Nothing
            mayTime = fmap (`addDuration` t) deadlineGap
        putDrawnState $
          Just (strs
              , (recordEvolution
              , frame
              , fmap (\d -> Deadline d redrawStatusPriority $ RedrawStatus deadlineFrame) mayTime)))
  recordFromStrs ref [_] (ClientState Ongoing (PlayLevel (Countdown n Running))) =
    informProgressively ref (mkRasterizedString (show n) grayGradient)
  recordFromStrs ref [unique] _ =
    if countChars unique < 3
      then
        informProgressively ref (mkRasterizedStringFromColorString unique)
      else
        drawStrs ref [unique]
  recordFromStrs ref strs _ = drawStrs ref strs
  drawStrs ref strs =
    liftIO mkRecordDraw >>= \e -> do
      flip runReaderT e $
        zipWithM_
          (flip drawAligned_)
          (map (\i -> mkCentered $ move (2*i) Down ref) [0..])
          strs
      liftIO (finalizeRecord e)
  informProgressively ref x = do
    face <- asks getSizedFace
    e <- liftIO mkRecordDraw
    liftIO (x face) >>= flip runReaderT e . drawVerticallyCentered ref
    liftIO $ finalizeRecord e
  go = \case
    ClientState Over Excluded ->
      inform "Joining..."
    ClientState Over Setup ->
      inform "..."
    ClientState Over (PlayLevel _) ->
      inform "Please wait..."
    ClientState Ongoing s -> case s of
      Excluded ->
        inform "A game is currently running on the server, please wait..."
      Setup ->
        return []
      PlayLevel (Countdown n Running) ->
        inform $ pack $ show n
      PlayLevel status ->
        statusMsg status
  statusMsg = \case
    New -> return [color "Waiting for game start..."]
    CancelledNoConnectedPlayer -> return [color "Game cancelled, all players left."]
    Paused disconnectedPlayers x -> -- TODO we could draw the previous status too (stack of status)
      intercalate ", " <$> showPlayerNames disconnectedPlayers >>= \them ->
        flip (++) [color "Game paused, waiting for [" <> them <> color "] to reconnect..."]  <$> statusMsg x
    Running -> return []
    WaitingForOthersToEndLevel stillPlaying ->
      intercalate ", " <$> showPlayerNames stillPlaying >>= \them ->
        return [color "Waiting for [" <> them <> color "] to finish..."]
    Countdown n x ->
      flip (++) [colored ("(" <> pack (show n) <> ")") neutralMessageColorFg] <$> statusMsg x
    OutcomeValidated o -> return $ map (flip colored' $ messageColor o) $ case o of
      (Lost reason) -> ["You lose", "(" <> reason <> ")"]
      Won           -> ["You win!"]
    WhenAllPressedAKey x (Just _) _ -> statusMsg x
    WhenAllPressedAKey x Nothing havePressed ->
      getMyShipId >>= maybe
        (error "todo")
        (\me -> flip (++) <$> maybe
          (error "logic")
          (\iHavePressed ->
            if iHavePressed
              then
                intercalate ", " <$> showPlayerNames (Map.keysSet $ Map.filter (== False) havePressed) >>= \them ->
                  return [color "Waiting for [" <> them <> color "] to press a key..."]
              else
                return [colored "Press a key to continue..." neutralMessageColorFg])
          (Map.lookup me havePressed)
          <*> statusMsg x)
  inform m = return [color m]
  color = flip colored' (messageColor Won)
  showPlayerNames = mapM showPlayerName . Set.toList
  showPlayerName x = maybe
    (colored (pack $ show x) white)
    (\(Player (PlayerName name) _ (PlayerColors c _)) -> colored name c)
    <$> getPlayer x
