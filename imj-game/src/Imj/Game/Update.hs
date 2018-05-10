{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Imj.Game.Update
      ( updateAppState
      , putClientState
      ) where

import           Imj.Prelude
import           Prelude(length)
import qualified Prelude as Unsafe(last)

import           Control.Exception.Base(throwIO)
import           Control.Monad.Reader.Class(MonadReader, asks)
import           Control.Monad.Reader(runReaderT)
import           Control.Monad.State.Strict(gets)
import           Data.Attoparsec.Text(parseOnly)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Text(pack, unpack, strip, uncons)
import qualified Data.Text as Text(length)
import           System.Exit(exitSuccess)
import           System.IO(putStrLn)

import           Imj.Control.Concurrent.AsyncGroups.Class
import           Imj.Game.Audio.Class
import           Imj.Game.Class
import           Imj.Game.Modify
import           Imj.Geo.Discrete.Types
import           Imj.Graphics.Class.DiscreteDistance
import           Imj.Graphics.Class.HasSizedFace
import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.Class.Render
import           Imj.Graphics.Color.Types
import           Imj.Graphics.ParticleSystem.Design.Update
import           Imj.Graphics.Screen
import           Imj.Graphics.UI.Animation.Types
import           Imj.ServerView.Types
import           Imj.Server.Class
import           Imj.Server.Types

import           Imj.Event
import           Imj.Game.Command
import           Imj.Game.Color
import           Imj.Game.Exceptions
import           Imj.Game.Level
import           Imj.Game.Priorities
import           Imj.Game.Status
import           Imj.Game.Show
import           Imj.Game.Timing
import           Imj.Graphics.Interpolation.Evolution
import           Imj.Graphics.RecordDraw
import           Imj.Graphics.Render.FromMonadReader
import           Imj.Graphics.Text.ColorString hiding(putStrLn)
import           Imj.Graphics.Text.RasterizedString
import           Imj.Graphics.UI.Animation
import           Imj.Graphics.UI.Chat
import           Imj.Graphics.UI.RectContainer
import           Imj.Iteration
import           Imj.Log
import           Imj.Server.Command

{-# INLINABLE updateAppState #-}
updateAppState :: (g ~ GameLogicT e, s ~ ServerT g
                 , StateValueT s ~ GameStateValue
                 , ServerCmdParser s
                 , MonadState (AppState g) m
                 , MonadReader e m, Client e, Render e, HasSizedFace e, AsyncGroups e, Audio e
                 , MonadIO m)
               => UpdateEvent g
               -- ^ The 'Event' that should be handled here.
               -> m ()
updateAppState (Right evt) = case evt of
  AppEvent e ->
    onClientOnlyEvent e
  ChatCmd chatCmd -> stateChat $ flip (,) () . runChat chatCmd
  SendChatMessage -> onSendChatMessage
  ToggleEventRecording ->
    error "should be handled by caller"
  Timeout (Deadline t _ (RedrawStatus f)) ->
    updateStatus (Just f) t
  Timeout (Deadline t _ AnimateUI) -> do
    _anim <$> getGameState >>= \a@(UIAnimation evolutions (UIAnimProgress _ it)) -> do
      let nextIt@(Iteration _ nextFrame) = nextIteration it
          worldAnimDeadline = fmap (flip addDuration t) $ getDeltaTime evolutions nextFrame
      putAnimation $ a { getProgress = UIAnimProgress worldAnimDeadline nextIt }
  Timeout (Deadline _ _ (AnimateParticleSystem key)) ->
    fmap systemTimePointToParticleSystemTimePoint (liftIO getSystemTime) >>= \tps ->
      gets game >>= \g ->
        putGame (g {
          gameParticleSystems =
            Map.updateWithKey
              (\_ (Prioritized p ps) -> fmap (Prioritized p) $ updateParticleSystem tps ps)
              key
              $ gameParticleSystems g })
  CanvasSizeChanged ->
    onTargetSize
  RenderingTargetChanged -> do
    onTargetChanged >>= either (liftIO . putStrLn) return
    onTargetSize
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
  Log Error txt ->
    error $ unpack txt
  Log msgLevel txt ->
    stateChat $ addMessage $ Information msgLevel txt
updateAppState (Left evt) = case evt of
  ServerAppEvt e ->
    onServerEvent e
  PlayMusic music instr ->
    asks playMusic >>= \f -> f music instr
  OnContent worldParameters ->
    putServerContent worldParameters
  RunCommand i cmd -> runClientCommand i cmd
  CommandError cmd err ->
    stateChat $ addMessage $ Information Warning $
      pack (show cmd) <> " failed:" <> err
  Reporting cmd ->
    stateChat $ addMessage $ Information Info $ pack $ chatShow cmd
  PlayerInfo notif i ->
    stateChat . addMessage . ChatMessage =<< toTxt i notif
  EnterState s ->
    putClientState $ ClientState Ongoing s
  ExitState s  ->
    putClientState $ ClientState Over s
  AllClients eplayers -> do
    asks sendToServer' >>= \f -> f $ ExitedState Excluded
    putClientState $ ClientState Over Excluded
    let p = Map.map mkPlayer eplayers
    putPlayers p
    stateChat $ addMessage $ ChatMessage $ welcome p
  ConnectionAccepted i -> do
    withAnim $ -- to make the frame take its initial size
      putGameConnection $ Connected i
  ConnectionRefused sn reason ->
    putGameConnection $ ConnectionFailed $
      "[" <>
      pack (show sn) <>
      "]" <>
      pack " is invalid:" <>
      reason
  Disconnected (ClientShutdown (Right ())) ->
    liftIO $ exitSuccess
  Disconnected (ClientShutdown (Left txt)) ->
    liftIO $ throwIO $ UnexpectedProgramEnd $ "Broken Client : " <> txt
  Disconnected s@(ServerShutdown _) ->
    liftIO $ throwIO $ UnexpectedProgramEnd $ "Disconnected by Server: " <> pack (show s)
  ServerError txt ->
    liftIO $ throwIO $ ErrorFromServer txt

 where

  toTxt i notif =
    (`mappend` colored (pack $ toTxt'' notif) chatMsgColor) . getPlayerUIName' <$> getPlayer i

  toTxt'' = \case
    Joins        -> " joins the game."
    WaitsToJoin  -> " is waiting to join the game."
    StartsGame   -> " starts the game."
    Done cmd@(Put _) ->
      " changed " ++ chatShow cmd
    Done cmd ->
      " " ++ chatShow cmd

{-# INLINABLE onTargetSize #-}
onTargetSize :: (GameLogic g
               , MonadState (AppState g) m
               , MonadReader e m, Canvas e
               , MonadIO m)
             => m ()
onTargetSize = getTargetSize >>= maybe
  (return ())
  (\sz -> do
    let screen@(Screen _ center) = mkScreen $ Just sz
    putCurScreen screen
    getGameState >>= \gs@(GameState mayG anim) -> do
      let ul = maybe center (_upperLeft . getViewport To screen) mayG
          newAnim = setPosition ul anim
      putGameState (gs { _anim = newAnim }))

{-# INLINABLE putClientState #-}
putClientState :: (MonadState (AppState g) m
                 , MonadReader e m, HasSizedFace e
                 , MonadIO m)
               => ClientState GameStateValue
               -> m ()
putClientState i = do
  gets game >>= \g -> putGame $ g { getClientState = i}
  liftIO getSystemTime >>= updateStatus Nothing

{-# INLINABLE updateStatus #-}
updateStatus :: (MonadState (AppState s) m
               , MonadReader e m, HasSizedFace e
               , MonadIO m)
             => Maybe (Frame, Int)
             -- ^ When Nothing, the current frame should be used.
             -> Time Point System
             -> m ()
updateStatus mayFrame t = gets game >>= \(Game state (Screen _ ref) _ _ drawnState' _ _ _ _ _) -> do
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
  updateStatusDeadline :: MonadState (AppState s) m => m ()
  updateStatusDeadline =
    zip [0..] . getDrawnClientState <$> gets game >>= mapM
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
    ClientState Over x -> case x of
      Excluded ->
        inform "Joining..."
      Included y -> case y of
        Setup ->
          inform "..."
        (PlayLevel _) ->
          inform "Please wait..."
    ClientState Ongoing x -> case x of
      Excluded ->
        inform "A game is currently running on the server, please wait..."
      Included y -> case y of
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
    WaitingForOthersToEndLevel _ ->
      -- We silent it because in real life with good connections we won't wait more than typical network latency,
      --  and we don't want to make an exception to the "display in full before transitionning" policy.
      -- Note that if the connection of another player is broken, we are in the 'Paused' branch
      -- NOTE (TODO) The server could test latencies of clients using pings, and take it into account.
      --    It would be interesting to find a technique to prevent clients from intentionally being slow to reply
      --    to such pings.
      return []
      -- Original unsilenced message:
      --intercalate ", " <$> showPlayerNames stillPlaying >>= \them ->
        --return [color "Waiting for [" <> them <> color "] to finish..."]
    Countdown n x ->
      flip (++) [colored ("(" <> pack (show n) <> ")") neutralMessageColorFg] <$> statusMsg x
    OutcomeValidated o -> return $ map (flip colored' $ messageColor o) $ case o of
      (Lost reason) -> ["You lose", "(" <> reason <> ")"]
      Won           -> ["You win!"]
    WhenAllPressedAKey x (Just _) _ -> statusMsg x
    WhenAllPressedAKey x Nothing havePressed ->
      getMyId >>= maybe
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
    (\(Player (ClientName name) _ (PlayerColors c _)) -> colored name c)
    <$> getPlayer x


onSendChatMessage :: (GameLogicT e ~ g
                    , ServerCmdParser (ServerT g)
                    , MonadState (AppState g) m
                    , MonadReader e m, Client e
                    , MonadIO m)
                  => m ()
onSendChatMessage =
  fmap strip (stateChat takeMessage) >>= \msg -> do
    f <- asks sendToServer'
    either
      (stateChat . addMessage . Information Warning . (<>) ("Error while parsing: " <> msg <> " : ") . pack)
      (f . OnCommand)
      $ parseOnly command msg
