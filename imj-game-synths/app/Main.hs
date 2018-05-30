{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
This is a multiplayer game where every player uses the keyboard as a synthesizer.

The enveloppe of the synthesizer can be tuned.

The music is shared between all players.
 -}

module Main where

import           Imj.Prelude

import           Control.Concurrent(forkIO, threadDelay)
import           Control.Concurrent.MVar.Strict(MVar, modifyMVar, modifyMVar_, newMVar, putMVar, takeMVar)
import           Control.DeepSeq(NFData)
import           Control.Monad.State.Strict(gets, execStateT)
import           Control.Monad.Reader(asks)
import           Data.Binary(Binary(..))
import           Data.Map.Internal(Map(..))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import           Data.Text(pack, Text)
import           Data.Vector.Unboxed(Vector)
import qualified Data.Vector.Unboxed as V
import           Data.Proxy(Proxy(..))
import           GHC.Generics(Generic)
import qualified Graphics.UI.GLFW as GLFW(Key(..), KeyState(..))

import           Imj.Categorized
import           Imj.ClientView.Types
import           Imj.Data.AlmostFloat
import           Imj.Event
import           Imj.Game.App(runGame)
import           Imj.Game.Draw
import           Imj.Game.Class
import           Imj.Game.Color
import           Imj.Game.Command
import           Imj.Game.KeysMaps
import           Imj.Game.Modify
import           Imj.Game.Show
import           Imj.Game.Status
import           Imj.Geo.Discrete.Types
import           Imj.Geo.Discrete.Resample
import           Imj.Graphics.Class.Positionable
import           Imj.Graphics.Class.UIInstructions
import           Imj.Graphics.Color
import           Imj.Graphics.Font
import           Imj.Graphics.Render.FromMonadReader
import           Imj.Graphics.Screen
import           Imj.Graphics.Text.ColorString(ColorString)
import qualified Imj.Graphics.Text.ColorString as CS
import           Imj.Graphics.Text.Render
import           Imj.Graphics.UI.RectContainer
import           Imj.Graphics.UI.Slider
import           Imj.Music.Types
import           Imj.Music.Analyze
import           Imj.Music.Piano
import           Imj.Music.Record
import           Imj.Server.Class hiding(Do)
import           Imj.Server.Connection
import           Imj.Server.Types
import           Imj.Server
import           Imj.Timing

main :: IO ()
main = runGame (Proxy :: Proxy SynthsGame)

data LoopId = LoopId {
    _loopCreator :: {-# UNPACK #-} !ClientId
  , _loopIndex :: {-# UNPACK #-} !Int
} deriving(Generic, Show, Ord, Eq)
instance Binary LoopId
instance NFData LoopId

data SynthsGame = SynthsGame {
    pianos :: !(Map ClientId PianoState)
  , pianoLoops :: !(Map SequencerId (Map LoopId PianoState))
  , instrument :: !Instrument
  , enveloppePlot :: [Vector Float]
  , editedIndex :: !Int
} deriving(Show)
instance UIInstructions SynthsGame where
  instructions color (SynthsGame _ _ instr _ idx) = case instr of
    SineSynthAHDSR env (AHDSR a h d r s) -> [envChoice] ++ ahdsrInstructions
     where
      envChoice = ConfigUI "Style" $ Choice $ map pack withCursor

      i = fromEnum env

      l =
        [ unwords ["ADHSR"]
        , unwords ["ADHSR", "Autorelease"]
        , unwords ["ADHSR", "Exp-decay"]
        , unwords ["ADHSR", "Exp-decay", "Autorelease"]
        ]

      cursor = "-> "

      withCursor =
        case splitAt i l of
          (_,[]) -> error "logic"
          (l1,e:l2) -> map ((++) "   ") l1 ++ ((cursor ++ e):map ((++) "   ") l2)

      ahdsrInstructions =
        [ ConfigUI "Attack" $ Discrete $
            mkSlider 0 a 50 3200 7
        , ConfigUI "Hold" $ Discrete $
            mkSlider 1 h 0 640 9
        , ConfigUI "Decay" $ Discrete $
            mkSlider 2 d 0 100000 7
        , ConfigUI "Sustain" $ Continuous $
            Slider (almost s) (almost 0.0) (almost 1.0) 7
              (right 3) (left 3) color Compact
        , ConfigUI "Release" $ Discrete $
            mkSlider 4 r 50 100000 7
        ]

      mkSlider x v min_ max_ steps =
        Slider v min_ max_ steps (right x) (left x) color Compact

      right x
        | x == idx `mod` 5 = '>'
        | otherwise = ' '
      left x
        | x == idx `mod` 5 = '<'
        | otherwise = ' '

    _ -> []

initialGame :: IO SynthsGame
initialGame = do
  let i = defaultInstr
  vec <- envelopeToVectors i
  return $ SynthsGame mempty mempty i vec 0

data SynthsMode =
    PlaySynth
  | EditSynth
  deriving(Show)

data SynthsServer = SynthsServer {
    srvSequencers :: !(Map SequencerId (MVar (Sequencer LoopId)))
  , nextSeqId :: !SequencerId
} deriving(Generic)
instance NFData SynthsServer

data SynthsClientView = SynthsClientView {
    _piano :: !PianoState
    -- ^ What is currently pressed, by the player (excluding what is played by loops)
  , _recording :: !Recording
    -- ^ The ongoing recording of what is being played, which can be used to create a 'Loop'
  , _nextLoopPianoIdx :: !Int
} deriving(Generic, Show)
instance NFData SynthsClientView

thePianoValue :: SynthsClientView -> PianoState
thePianoValue (SynthsClientView x _ _) = x

-- | The server will also send 'PlayMusic' events, which are generic events.
data SynthsServerEvent =
    PianoValue !(Either ClientId (SequencerId, LoopId)) !PianoState
  | NewLine {-# UNPACK #-} !SequencerId {-# UNPACK #-} !LoopId
  deriving(Generic, Show)
instance DrawGroupMember SynthsServerEvent where
  exclusivityKeys = \case
    NewLine {} -> mempty
    PianoValue {} -> mempty -- we do this is so that interleaving 'PianoValue' events with 'PlayMusic' events
                           -- will still allow multiple play music events to be part of the same frame.
                           -- It is a bit of a hack, the better solution would be to handle audio events
                           -- client-side as soon as they are received (in the listening thread) (TODO).

instance Categorized SynthsServerEvent
instance NFData SynthsServerEvent
instance Binary SynthsServerEvent

data SynthClientEvent =
    PlayNote !Music
  | WithMultiLineSequencer {-# UNPACK #-} !SequencerId
  | WithMonoLineSequencer
  | ForgetCurrentRecording
  deriving(Show,Generic)
instance Binary SynthClientEvent
instance Categorized SynthClientEvent

data SynthsGameEvent =
    ChangeInstrument !Instrument
  | ChangeEditedFeature {-# UNPACK #-} !Int
  deriving(Show)
instance Categorized SynthsGameEvent
instance DrawGroupMember SynthsGameEvent where
  exclusivityKeys _ = mempty

instance GameExternalUI SynthsGame where

  gameWindowTitle = const "Play some music!"

  -- NOTE 'getViewport' is never called unless 'putIGame' was called once.
  getViewport _ (Screen _ center) SynthsGame{} =
    mkCenteredRectContainer center $ Size 45 100

data SynthsStatefullKeys
instance GameStatefullKeys SynthsGame SynthsStatefullKeys where

  mapStateKey _ GLFW.Key'Space GLFW.KeyState'Pressed _ _ _ =
    return $ Just $ CliEvt $ ClientAppEvt WithMonoLineSequencer
  mapStateKey _ GLFW.Key'F1 GLFW.KeyState'Pressed _ _ _ =
    return $ Just $ CliEvt $ ClientAppEvt $ WithMultiLineSequencer $ SequencerId 1
  mapStateKey _ GLFW.Key'F2 GLFW.KeyState'Pressed _ _ _ =
    return $ Just $ CliEvt $ ClientAppEvt $ WithMultiLineSequencer $ SequencerId 2
  mapStateKey _ GLFW.Key'F3 GLFW.KeyState'Pressed _ _ _ =
    return $ Just $ CliEvt $ ClientAppEvt $ WithMultiLineSequencer $ SequencerId 3
  mapStateKey _ GLFW.Key'F4 GLFW.KeyState'Pressed _ _ _ =
    return $ Just $ CliEvt $ ClientAppEvt $ WithMultiLineSequencer $ SequencerId 4
  mapStateKey _ GLFW.Key'F10 GLFW.KeyState'Pressed _ _ _ =
    return $ Just $ CliEvt $ ClientAppEvt ForgetCurrentRecording
  mapStateKey _ k st _ _ g = maybe
    (return Nothing)
    (\(SynthsGame _ _ instr _ idx) -> maybe
      (case k of
        GLFW.Key'Enter -> case st of
          GLFW.KeyState'Pressed ->
            return $ Just $ Evt $ AppEvent $ ChangeInstrument $ cycleEnvelope' instr
          _ -> return Nothing
        _ -> return $ CliEvt . ClientAppEvt . PlayNote <$> n instr)
      (\dir -> case instr of
          SineSynthAHDSR{} -> case st of
            GLFW.KeyState'Pressed -> configureInstrument
            GLFW.KeyState'Repeating -> configureInstrument
            _ -> return Nothing
           where
            configureInstrument = case dir of
              LEFT ->  return $ Just $ Evt $ AppEvent $ ChangeInstrument $ changeIntrumentValue instr idx (-1)
              RIGHT -> return $ Just $ Evt $ AppEvent $ ChangeInstrument $ changeIntrumentValue instr idx 1
              Up   -> return $ Just $ Evt $ AppEvent $ ChangeEditedFeature $ idx - 1
              Down -> return $ Just $ Evt $ AppEvent $ ChangeEditedFeature $ idx + 1
          _ -> return Nothing)
        $ isArrow k)
    $ _game $ getGameState' g
   where
    n instr = maybe Nothing (\noteSpec -> case st of
      GLFW.KeyState'Pressed -> Just $ StartNote noteSpec 1
      GLFW.KeyState'Released -> Just $ StopNote noteSpec
      GLFW.KeyState'Repeating -> Nothing) $ fmap (\f -> f instr) $ keyToNote k

    keyToNote = \case
      -- NOTE GLFW uses the US keyboard layout to name keys: https://en.wikipedia.org/wiki/British_and_American_keyboards
      -- lower keys
      GLFW.Key'Z -> Just $ NoteSpec Do $ noOctave - 1
      GLFW.Key'S -> Just $ NoteSpec Réb $ noOctave - 1
      GLFW.Key'X -> Just $ NoteSpec Ré $ noOctave - 1
      GLFW.Key'D -> Just $ NoteSpec Mib $ noOctave - 1
      GLFW.Key'C -> Just $ NoteSpec Mi $ noOctave - 1
      GLFW.Key'V -> Just $ NoteSpec Fa $ noOctave - 1
      GLFW.Key'G -> Just $ NoteSpec Solb $ noOctave - 1
      GLFW.Key'B -> Just $ NoteSpec Sol $ noOctave - 1
      GLFW.Key'H -> Just $ NoteSpec Lab $ noOctave - 1
      GLFW.Key'N -> Just $ NoteSpec La $ noOctave - 1
      GLFW.Key'J -> Just $ NoteSpec Sib $ noOctave - 1
      GLFW.Key'M -> Just $ NoteSpec Si $ noOctave - 1
      GLFW.Key'Comma -> Just $ NoteSpec Do $ noOctave + 0
      GLFW.Key'L -> Just $ NoteSpec Réb $ noOctave + 0
      GLFW.Key'Period -> Just $ NoteSpec Ré $ noOctave + 0
      GLFW.Key'Semicolon -> Just $ NoteSpec Mib $ noOctave + 0
      GLFW.Key'Slash -> Just $ NoteSpec Mi $ noOctave + 0
      -- upper keys
      GLFW.Key'Q -> Just $ NoteSpec Do $ noOctave + 0
      GLFW.Key'2 -> Just $ NoteSpec Réb $ noOctave + 0
      GLFW.Key'W -> Just $ NoteSpec Ré $ noOctave + 0
      GLFW.Key'3 -> Just $ NoteSpec Mib $ noOctave + 0
      GLFW.Key'E -> Just $ NoteSpec Mi $ noOctave + 0
      GLFW.Key'R -> Just $ NoteSpec Fa $ noOctave + 0
      GLFW.Key'5 -> Just $ NoteSpec Solb $ noOctave + 0
      GLFW.Key'T -> Just $ NoteSpec Sol $ noOctave + 0
      GLFW.Key'6 -> Just $ NoteSpec Lab $ noOctave + 0
      GLFW.Key'Y -> Just $ NoteSpec La $ noOctave + 0
      GLFW.Key'7 -> Just $ NoteSpec Sib $ noOctave + 0
      GLFW.Key'U -> Just $ NoteSpec Si $ noOctave + 0
      GLFW.Key'I -> Just $ NoteSpec Do $ noOctave + 1
      GLFW.Key'9 -> Just $ NoteSpec Réb $ noOctave + 1
      GLFW.Key'O -> Just $ NoteSpec Ré $ noOctave + 1
      GLFW.Key'0 -> Just $ NoteSpec Mib $ noOctave + 1
      GLFW.Key'P -> Just $ NoteSpec Mi $ noOctave + 1
      GLFW.Key'LeftBracket -> Just $ NoteSpec Fa $ noOctave + 1
      GLFW.Key'Equal -> Just $ NoteSpec Solb $ noOctave + 1
      GLFW.Key'RightBracket -> Just $ NoteSpec Sol $ noOctave + 1
      _ -> Nothing

    cycleEnvelope' = \case
      SineSynthAHDSR env ahdsr ->
        SineSynthAHDSR (cycleEnvelope env) ahdsr
      other -> other

    changeIntrumentValue instr idx inc =
      case instr of
        SineSynthAHDSR env p@(AHDSR a h d r s) -> case idx `mod` 5 of
          0 -> SineSynthAHDSR env p {ahdsrAttack = a + inc}
          1 -> SineSynthAHDSR env p {ahdsrHold = h + inc}
          2 -> SineSynthAHDSR env p {ahdsrDecay = d + inc}
          3 -> SineSynthAHDSR env p {ahdsrSustain = s + (fromIntegral inc) / 10.0}
          4 -> SineSynthAHDSR env p {ahdsrRelease = r + inc}
          _ -> error "logic"
        _ -> instr

defaultInstr :: Instrument
defaultInstr = SineSynthAHDSR AHPropDerDSR_AutoReleaseAfterDecay bell

instance GameLogic SynthsGame where

  type ServerT SynthsGame = SynthsServer
  type StatefullKeysT SynthsGame = SynthsStatefullKeys
  type ClientOnlyEvtT SynthsGame = SynthsGameEvent

  mapInterpretedKey _ _ _ = return Nothing

  onClientOnlyEvent e = do
    mayNewEnvVectors <-
      case e of
        ChangeInstrument i -> Just <$> liftIO (envelopeToVectors i)
        _ -> return Nothing
    getIGame >>= maybe (liftIO initialGame) return >>= \g -> withAnim $ putIGame $ case e of
      ChangeInstrument i -> g {instrument = i, enveloppePlot = fromMaybe (error "logic") mayNewEnvVectors}
      ChangeEditedFeature i -> g {editedIndex = i}

  onServerEvent e =
    -- TODO force withAnim when using putIGame ?
    getIGame >>= maybe (liftIO initialGame) return >>= \g -> withAnim $ putIGame $ case e of
      NewLine seqId loopId ->
        g { pianoLoops = Map.insertWith Map.union seqId (Map.singleton loopId mkEmptyPiano) $ pianoLoops g }
      PianoValue creator x -> either
        (\i ->
          g {pianos = Map.insert i x $ pianos g})
        (\(seqId,loopId) ->
          g {pianoLoops = Map.insertWith Map.union seqId (Map.singleton loopId x) $ pianoLoops g})
        creator

instance GameDraw SynthsGame where

  drawBackground (Screen _ center@(Coords _ centerC)) g@(SynthsGame pianoClients pianoLoops_ _ curves _) = do
    case curves of
      [] -> return ()
      [ahds,r] -> do
        let coordsEnv = move 2 LEFT $ move 18 Up center
            szAHDS@(Size _ (Length wAHDS)) = Size 20 30
            szR = Size 20 20
        drawEnv ahds coordsEnv szAHDS
        drawEnv r    (move wAHDS RIGHT coordsEnv) szR
      _ -> error "logic"
    let infos =
          ["Play the synth with your keyboard!"
          ,"Press [Enter] to cycle the envelope style."
          ,"Use 4 arrows to change envelope parameters."]
    foldM
      (flip $ drawAligned . flip CS.colored configFgColor . pack)
      (mkCentered $ move 25 LEFT $ move 18 Up center)
      infos >>= \(Alignment _ c) ->
       drawInstructions g (move 15 LEFT $ move 1 Down c) >>= \ref -> do
        ref1 <- showPianos
          "Players"
          showPlayerName
          pianoClients >>= drawPiano (move 1 Down ref)
        foldM_
          (\r ((SequencerId seqId), seqPianoLoops) -> do
            showPianos
              ("Sequence " <> pack (show seqId))
              (\(LoopId creator idx) -> flip (<>) (" " <> CS.colored (pack (show idx)) (rgb 2 2 2)) <$> showPlayerName creator)
              seqPianoLoops >>= drawPiano r)
            ref1
            $ Map.assocs pianoLoops_
        return center

   where

    drawPiano (Coords r _) allStrs = do
      let maxL = fromMaybe 0 $ maximumMaybe $ map CS.countChars allStrs
          right = move (quot maxL 2) RIGHT $ Coords r centerC
      (Alignment _ res) <- foldM
        (\a str -> drawAligned str a)
        (mkRightAlign right)
        allStrs
      return res


    drawEnv c ul (Size h' w) = do
      let h = fromIntegral h'
          ll = move h Down ul
          nSamples = V.length c
          --resampled = resampleMinMaxLinear (V.toList c) nSamples $ fromIntegral w
          -- TODO do not recompute resampleMinMaxLogarithmic at each render frame
          resampled = resampleMinMaxLogarithmic (V.toList c) nSamples $ fromIntegral w
          heights (MinMax a b) = [round (a*fromIntegral h)..round (b*fromIntegral h)]
      mapM_
        (\(i,mm) ->
          mapM_
            (\j -> drawGlyph (textGlyph '+') (translate ll $ Coords (-j) i) configColors)
            $ heights mm)
        $ zip [0..] resampled
      drawAligned_ (CS.colored (pack $ show nSamples) configFgColor) (mkRightAlign $ move (fromIntegral w - 1) RIGHT $ move 2 Down ll)
      drawAt (CS.colored "0" configFgColor) (move 2 Down ll)

instance ServerInit SynthsServer where

  type ClientViewT SynthsServer = SynthsClientView

  mkInitialState _ = return ((), SynthsServer mempty $ SequencerId 10) -- the first 9 sequencers are reserved

  mkInitialClient = SynthsClientView mkEmptyPiano mkEmptyRecording 0

instance ServerInParallel SynthsServer


instance Server SynthsServer where

  type ServerEventT SynthsServer = SynthsServerEvent

  greetNewcomer' =
    -- Send the currently started notes so that the newcomer
    -- hears exactly what other players are hearing.

    -- In this loop we have no race condition because to modify currently pressed keys, a client
    -- handler must take the MVar lock of server state which we have taken here.
    map (fmap unClientView) . Map.assocs <$> gets clientsMap >>=
      return . concatMap
        (\(i,(SynthsClientView piano _ _)) -> pianoEvts (Left i) piano)

instance ServerClientLifecycle SynthsServer where

  onStartClient _ =
    Map.assocs <$> getsState srvSequencers >>=
      mapM_
        (\(seqId,s) -> do
          se@(Sequencer _ _ musLines) <- liftIO $ takeMVar s
          mapM_
            (\(loopId,(MusicLine _ v)) -> do
              piano <- liftIO $ takeMVar v
              case pianoEvts (Right (seqId, loopId)) piano of
                [] -> return ()
                evts@(_:_) -> notifyClientN' evts
              liftIO $ putMVar v piano)
            $ Map.assocs musLines
          liftIO $ putMVar s se)

  clientCanJoin _ = do
    -- A client has just connected, we make it be part of the current game:
    notifyClient' $ EnterState $ Included $ PlayLevel Running
    return True

instance ServerClientHandler SynthsServer where

  type StateValueT  SynthsServer = GameStateValue -- This is required

  type ClientEventT SynthsServer = SynthClientEvent

  handleClientEvent e = case e of
    PlayNote n -> do
      onRecordableNote n >>= notifyEveryoneN'
      return []
    ForgetCurrentRecording -> do
      adjustClient_ $ \s -> s {_recording = mkEmptyRecording }
      return []
    WithMonoLineSequencer ->
      usingRecording $ \loopId recording now ->
        addSequencer Nothing loopId recording now
    WithMultiLineSequencer seqId ->
      usingRecording $ \loopId recording now ->
        Map.lookup seqId . srvSequencers <$> gets unServerState >>= maybe
          (addSequencer (Just seqId) loopId recording now)
          (\sequencer ->
            liftIO (modifyMVar sequencer (\s@(Sequencer start _ _) ->
              liftIO (insertRecording recording loopId s) >>= return . either
                (\err -> (s, Left err))
                (\(s',mus) -> (s', Right (s', mus, start...now))))) >>= either
                (\msg -> do
                  notifyClient' $ Warn msg
                  return [])
                (\((Sequencer start _ _), (MusicLine mus pianoV), progress) -> do
                  notifyEveryone $ NewLine seqId loopId
                  return
                    [ (\v -> startLoopThreadAt
                          (\m -> do
                            (nChanged',newPiano') <- liftIO $ modifyMVar pianoV $ \piano ->
                              let (nChanged,newPiano) = modPiano m piano
                              in return $ (newPiano, (nChanged,newPiano))
                            when (nChanged' > 0) $
                              modifyMVar_ v $ execStateT $ playLoopMusic seqId loopId newPiano' m)
                          start progress mus)
                    ]))

   where

    onRecordableNote :: (MonadIO m, MonadState (ServerState SynthsServer) m, MonadReader ConstClientView m)
                       => Music
                       -> m [ServerEvent SynthsServer]
    onRecordableNote n = do
      cid <- asks clientId
      fmap (_piano . unClientView) . Map.lookup cid <$> gets clientsMap >>= maybe
        (return []) -- should never happen
        (\piano -> do
          let (countChangedNotes, piano') = modPiano n piano
          if countChangedNotes == 0
            then
              return []
            else do
              t <- liftIO getSystemTime
              creator <- asks clientId
              newpiano <- _piano <$> adjustClient (\s@(SynthsClientView _ recording _) ->
                    s { _piano = piano'
                      , _recording = recordMusic t recording n })
              return
                [ ServerAppEvt $ PianoValue (Left creator) newpiano
                , PlayMusic n
                ])

    playLoopMusic :: (MonadState (ServerState SynthsServer) m, MonadIO m)
                  => SequencerId -> LoopId -> PianoState -> Music -> m ()
    playLoopMusic seqId loopId newPiano n =
      notifyEveryoneN'
        [ ServerAppEvt $ PianoValue (Right (seqId,loopId)) newPiano
        , PlayMusic n
        ]

    addSequencer maySeqId loopId recording now =
      liftIO (mkSequencerFromRecording loopId recording now) >>= either
        (\msg -> do
          notifyClient' $ Warn msg
          return [])
        (\sequencerV -> do
          sequencer <- liftIO $ newMVar sequencerV
          seqId <- modifyState' $ \(SynthsServer m i) ->
            let (sid,succI) = maybe (i,succ i) (\j -> (j,i)) maySeqId
            in (sid,SynthsServer (Map.insert sid sequencer m) succI)
          notifyEveryone $ NewLine seqId loopId
          return
            [ (\v -> forever $ do
                now' <- getSystemTime
                modifyMVar sequencer (\(Sequencer _ a b) -> do
                  let s = (Sequencer now' a b)
                  return (s,s))
                  >>= \(Sequencer startTime duration vecs) -> do
                    forM_ (Map.assocs vecs) $ \(lid,(MusicLine vec pianoV)) ->
                      void $ forkIO $
                        playOnce
                          (\m -> do
                            (nChanged',newPiano') <- modifyMVar pianoV $ \piano ->
                              let (nChanged,newPiano) = modPiano m piano
                              in return $ (newPiano, (nChanged,newPiano))
                            when (nChanged' > 0) $
                              modifyMVar_ v $ execStateT $ playLoopMusic seqId lid newPiano' m)
                          vec
                          startTime
                    threadDelay $ fromIntegral $ toMicros $ duration)])

    usingRecording x = do
      cid <- asks clientId
      fmap (_piano . unClientView) . Map.lookup cid <$> gets clientsMap >>= maybe
        (return [])
        (\piano -> do
          concat <$> forM (silencePiano piano) onRecordableNote >>= notifyEveryoneN'
          fmap (_recording . unClientView) . Map.lookup cid <$> gets clientsMap >>= maybe
            (return [])
            (\recording -> do
              creator <- asks clientId
              (SynthsClientView _ _ idx) <- adjustClient
                (\(SynthsClientView _ _ loopIdx) ->
                  SynthsClientView
                    mkEmptyPiano
                    mkEmptyRecording
                    $ loopIdx + 1)
              let loopId = LoopId creator $ idx - 1
              liftIO getSystemTime >>= x loopId recording))

pianoEvts :: Either ClientId (SequencerId,LoopId) -> PianoState -> [ServerEvent SynthsServer]
pianoEvts idx v@(PianoState m) =
  ServerAppEvt (PianoValue idx v) :
  concatMap
    (\(note,n) ->
      replicate n $ PlayMusic (StartNote note 1))
    (Map.assocs m)

instance ServerCmdParser SynthsServer


{-# INLINABLE showPianos #-}
showPianos :: MonadReader e m
           => Text
           -> (a -> m ColorString)
           -> Map a PianoState
           -> m [ColorString]
showPianos _ _ Tip = return []
showPianos title showKey m = do
  let minNote = noteToMidiPitch' Do $ noOctave - 1
      maxNote = noteToMidiPitch' Sol $ noOctave + 1
  showArray (Just (CS.colored title $ rgb 2 1 2,"")) <$> mapM
    (\(i,piano) -> flip (,) (CS.colored (pack $ showKeys minNote maxNote piano) $ rgb 3 1 2) <$> showKey i)
    (Map.assocs m)

showKeys :: MidiPitch
         -- ^ From
         -> MidiPitch
         -- ^ To
         -> PianoState
         -> String
showKeys from to (PianoState m) =
  go [] [from..to] $ Set.toAscList $ Map.keysSet m
 where
  go l [] _ = reverse l
  go l (k:ks) remainingPressed =
    case remainingPressed of
      [] -> go' freeChar remainingPressed
      (NoteSpec pressedNoteName pressedNoteOctave _):ps ->
        if (pressedNoteName, pressedNoteOctave) == midiPitchToNoteAndOctave k
          then
            go' pressedChar ps
          else
            go' freeChar remainingPressed
   where
    go' c remPressed = go (maybeToList space ++ [c] ++ l) ks remPressed

    space = case ks of
      [] -> Nothing
      s:_ -> case fst $ midiPitchToNoteAndOctave s of
        Fa -> Just ' '
        Do -> Just ' '
        _ -> Nothing

    freeChar
      | naturalPitch k = '-'
      | otherwise = '*'

    pressedChar
      | naturalPitch k = '_'
      | otherwise = '.'
