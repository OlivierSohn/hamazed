{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

module Imj.Game.Hamazed.Loop.Update
      ( updateAppState
      , sendToServer
      , send'ToServer
      ) where

import           Imj.Prelude
import           Prelude(length)
import qualified Prelude as Unsafe(last)

import           Control.Concurrent(forkIO)
import           Control.Concurrent.Async(withAsync)
import           Control.Exception.Base(throwIO)
import           Control.Monad.Reader.Class(MonadReader, asks)
import           Control.Monad.Reader(runReaderT)

import           Data.Attoparsec.Text(parseOnly)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Text(pack, unpack, strip, uncons)
import qualified Data.Text as Text(length, intercalate)
import           System.Exit(exitSuccess)
import           System.IO(putStrLn)

import           Imj.Client.Class
import           Imj.Game.Hamazed.World.Space.Types
import           Imj.Game.Hamazed.Network.Types
import           Imj.Game.Hamazed.State.Types
import           Imj.Game.Hamazed.Types
import           Imj.Graphics.Color.Types

import           Imj.Game.Hamazed.Color
import           Imj.Game.Hamazed.Command
import           Imj.Game.Hamazed.Env
import           Imj.Game.Hamazed.Loop.Create
import           Imj.Game.Hamazed.Loop.Event
import           Imj.Game.Hamazed.Loop.Event.Priorities
import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Game.Hamazed.Network.Class.AsyncGroups
import           Imj.Game.Hamazed.Sound
import           Imj.Game.Hamazed.World
import           Imj.Game.Hamazed.World.Create
import           Imj.Game.Hamazed.World.Ship
import           Imj.Graphics.Class.HasSizedFace
import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.RecordDraw
import           Imj.Graphics.Render.FromMonadReader
import           Imj.Graphics.Text.ColorString hiding(putStrLn)
import           Imj.Graphics.Text.RasterizedString
import           Imj.Graphics.Text.Render
import           Imj.Graphics.UI.RectContainer
import           Imj.Random.MWC.Parallel(mkOneGenPerCapability)
import           Imj.Music hiding(Do)

{-# INLINABLE updateAppState #-}
updateAppState :: (MonadState AppState m
                 , MonadReader (Env i) m
                 , MonadIO m)
               => UpdateEvent HamazedServerState Event
               -- ^ The 'Event' that should be handled here.
               -> m ()
updateAppState (Right evt) = case evt of
  Interrupt Help -> error "not implemented"
  Log Error txt -> error $ unpack txt
  Log msgLevel txt -> stateChat $ addMessage $ Information msgLevel txt
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
  PlayProgram i -> liftIO $ playAtTempo (Wind i) 120 [notes| vdo vsol do sol ^do|]
  ToggleEventRecording -> error "should be handled by caller"
updateAppState (Left evt) = case evt of
  ServerAppEvt e -> case e of
    RunCommand i cmd -> runClientCommand i cmd
    CommandError cmd err ->
      stateChat $ addMessage $ Information Warning $
        pack (show cmd) <> " failed:" <> err
    Reporting cmd ->
      stateChat $ addMessage $ Information Info $ showReport cmd
    PlayMusic music instr -> liftIO $ play music instr
    WorldRequest wid arg -> case arg of
      GetGameState ->
        mkGameStateEssence wid <$> getGameState >>= sendToServer . CurrentGameState wid
      Build dt spec ->
        asks sendToServer' >>= \send -> asks belongsTo' >>= \ownedByRequest ->
          void $ liftIO $ forkIO $ flip withAsync (`ownedByRequest` wid) $
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
      Cancel -> asks cancel' >>= \cancelAsyncsOwnedByRequest -> cancelAsyncsOwnedByRequest wid
    ChangeLevel levelEssence worldEssence wid ->
      getGameState >>= \state@(GameState _ _ _ _ _ _ (Screen sz _) viewMode names) ->
        mkInitialState levelEssence worldEssence (Just wid) names viewMode sz (Just state)
          >>= putGameState
    PutGameState (GameStateEssence worldEssence shotNums levelEssence) wid ->
      getGameState >>= \state@(GameState _ _ _ _ _ _ (Screen sz _) viewMode names) ->
        mkIntermediateState shotNums levelEssence worldEssence (Just wid) names viewMode sz (Just state)
          >>= putGameState
    OnWorldParameters worldParameters ->
      putWorldParameters worldParameters
    GameEvent (PeriodicMotion accelerations shipsLosingArmor) ->
      onMove accelerations shipsLosingArmor
    GameEvent (LaserShot dir shipId) -> do
      liftIO laserSound
      onLaser shipId dir Add
    MeetThePlayers eplayers -> do
      let p = Map.map mkPlayer eplayers
      putPlayers p
      stateChat $ addMessage $ ChatMessage $ welcome p
    PlayerInfo notif i ->
      stateChat . addMessage . ChatMessage =<< toTxt i notif
    GameInfo notif ->
      stateChat $ addMessage $ ChatMessage $ toTxt' notif
    EnterState s ->
      putClientState $ ClientState Ongoing s
    ExitState s  ->
      putClientState $ ClientState Over s
  ConnectionAccepted i -> do
    sendToServer $ ExitedState Excluded
    putClientState $ ClientState Over Excluded
    putGameConnection $ Connected i
  ConnectionRefused reason ->
    putGameConnection $ ConnectionFailed reason
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
    Done cmd@(Put _) ->
      " changed " <> showReport cmd
    Done cmd ->
      " " <> showReport cmd

  toTxt' (LevelResult (LevelNumber n) (Lost reason)) =
    colored ("- Level " <> pack (show n) <> " was lost : " <> reason <> ".") chatMsgColor
  toTxt' (LevelResult (LevelNumber n) Won) =
    colored ("- Level " <> pack (show n) <> " was won!") chatWinColor
  toTxt' GameWon =
    colored "- The game was won! Congratulations!" chatWinColor
  toTxt' (CannotCreateLevel errs n) =
    colored ( Text.intercalate "\n" errs <> "\nHence, the server cannot create level " <> pack (show n)) red

  showReport (Put (ColorSchemeCenter c)) =
    ("color scheme center:" <>) $ pack $ show $ color8CodeToXterm256 c
  showReport (Put (WorldShape shape)) =
    ("world shape:" <>) $ pack $ show shape
  showReport (Succ x) =
    "incremented " <> pack (show x)
  showReport (Pred x) =
    "decremented " <> pack (show x)

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
               , MonadReader (Env i) m
               , MonadIO m)
             => HamazedClientEvent
             -> m ()
sendToServer = send'ToServer . ClientAppEvt

{-# INLINABLE send'ToServer #-}
send'ToServer :: (MonadState AppState m
               , MonadReader (Env i) m
               , MonadIO m)
             => ClientEvent HamazedServerState
             -> m ()
send'ToServer e =
  asks sendToServer' >>= \f -> f e

onSendChatMessage :: (MonadState AppState m
                    , MonadReader (Env i) m
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
        (sendToServer . (\case
            ServerRep rep -> Report rep
            ServerCmd cmd -> Do cmd
            ClientCmd cmd -> RequestApproval cmd)))
      p

{-# INLINABLE onLaser #-}
onLaser :: (MonadState AppState m
          , MonadReader (Env i) m
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
         , MonadReader (Env i) m
         , MonadIO m)
       => Map ShipId (Coords Vel)
       -> Set ShipId
       -> m ()
onMove accelerations shipsLosingArmor = do
  moveWorld accelerations shipsLosingArmor
  onHasMoved

{-# INLINABLE onHasMoved #-}
onHasMoved :: (MonadState AppState m
             , MonadReader (Env i) m
             , MonadIO m)
           => m ()
onHasMoved =
  liftIO getSystemTime >>= shipParticleSystems >>= addParticleSystems >> getGameState
    >>= \(GameState world@(World balls ships _ _ _ _) f shotNums (Level level@(LevelEssence _ target _) finished) anim b o m n) -> do
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
      putGameState $ assert (isFinished anim) $ GameState newWorld f shotNums newLevel anim b o m n
      when numbersChanged checkSums
      updateShipsText

{-# INLINABLE updateUIAnim #-}
updateUIAnim :: (MonadState AppState m
               , MonadReader (Env i) m
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
             => Maybe (Frame, Int)
             -- ^ When Nothing, the current frame should be used.
             -> Time Point System
             -> m ()
updateStatus mayFrame t = gets game >>= \(Game state (GameState _ _ _ _ _ drawnState' (Screen _ ref) _ _) _ _ _ _) -> do
  let drawnState = zip [0 :: Int ..] drawnState'
  newStrs <- zip [0 :: Int ..] <$> go state
  -- return the same evolution when the string didn't change.
  part1 <- forM
    (zip newStrs drawnState)
    (\((i,newStr),(_,(curStr,curLine@(AnimatedLine (Evolution (Successive s) _ _ _) _ _)))) ->
      if newStr == curStr
        then
          return (curStr,curLine)
        else do
          let mayPrevRecord = case s of
                [] -> Nothing
                _:_ -> Just $ Unsafe.last s
          evolutionStart <- flip fromMaybe mayPrevRecord <$> liftIO mkZeroRecordDraw
          evolutionEnd <- recordFromStrs (move (2*i) Down ref) newStr
          let ev = mkEvolutionEaseQuart (Successive [evolutionStart,evolutionEnd]) $ fromSecs 1
          return (newStr, AnimatedLine ev 0 Nothing))
  part2 <- forM
    (drop (length drawnState) newStrs)
    (\(i,newStr) -> do
      evolutionStart <- liftIO mkZeroRecordDraw
      evolutionEnd <- recordFromStrs (move (2*i) Down ref) newStr
      let ev = mkEvolutionEaseQuart (Successive [evolutionStart,evolutionEnd]) $ fromSecs 1
      return (newStr, AnimatedLine ev 0 Nothing))
  part3 <- catMaybes <$> forM
    (drop (length newStrs) drawnState)
    (\(_,(oldStr,oldRec@(AnimatedLine (Evolution (Successive s) _ _ _) _ deadline))) ->
      if oldStr == ""
        then
          return $ maybe Nothing (const $ Just (oldStr,oldRec)) deadline
        else do
          let mayPrevRecord = case s of
                [] -> Nothing
                _:_ -> Just $ Unsafe.last s
          evolutionStart <- flip fromMaybe mayPrevRecord <$> liftIO mkZeroRecordDraw
          evolutionEnd <- liftIO mkZeroRecordDraw
          let ev = mkEvolutionEaseInQuart (Successive [evolutionStart,evolutionEnd]) $ fromSecs 0.5
          return $ Just ("", AnimatedLine ev 0 Nothing))
  putDrawnState $ part1 ++ part2 ++ part3
  updateStatusDeadline
 where
  updateStatusDeadline :: MonadState AppState m => m ()
  updateStatusDeadline =
    zip [0..] . getDrawnClientState <$> getGameState >>= mapM
      (\(i, (str, AnimatedLine recordEvolution curFrame _)) -> do
        let frame = fromMaybe curFrame $ maybe
              Nothing
              (\(targetFrame,j) -> if i==j then Just targetFrame else Nothing)
              mayFrame
            minDt = fromSecs 0.015
            significantDeadline f sofar = maybe
              (succ f, sofar)
              (\dt ->
                let newDuration = dt |+| fromMaybe zeroDuration sofar
                in if newDuration > minDt
                    then
                      (succ f, Just newDuration)
                    else
                      significantDeadline (succ f) $ Just newDuration)
              $ getDeltaTimeToNextFrame recordEvolution f
            (deadlineFrame, deadlineGap) = significantDeadline frame Nothing
            deadline = fmap (\d -> Deadline (addDuration d t) redrawStatusPriority $ RedrawStatus (deadlineFrame,i)) deadlineGap
        return (str, AnimatedLine recordEvolution frame deadline))
      >>= putDrawnState
  recordFromStrs ref (ColorString [(txt,_)])
    | Text.length txt == 1 =
        let (c,_) = fromMaybe (error "logic") $ uncons txt
        in informProgressively ref $ mkRasterizedString [c] grayGradient
  recordFromStrs ref unique
    | countChars unique < 3 =
        informProgressively ref $ mkRasterizedStringFromColorString unique
    | otherwise =
        liftIO mkRecordDraw >>= \e -> do
          flip runReaderT e $ drawAligned_ unique $ mkCentered ref
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
