{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}

{-|
This is a multiplayer game where every player uses the keyboard as a synthesizer.

The enveloppe of the synthesizer can be tuned.

The music is shared between all players.
-}

module Main where

import           Imj.Prelude
import           Prelude(length, putStrLn, print)
--import qualified Debug.Trace as Debug
import           Codec.Midi hiding(key, Key)
import           Control.Concurrent(forkIO, threadDelay)
import           Control.Concurrent.MVar.Strict(MVar, modifyMVar, modifyMVar_, newMVar, putMVar, takeMVar)
import           Control.DeepSeq(NFData)
import           Control.Monad.State.Strict(gets, execStateT, state)
import           Control.Monad.Reader(asks)
import           Data.Binary(Binary(..), encode, decodeOrFail)
import           Data.Bits (shiftR, shiftL, (.&.))
import qualified Data.ByteString.Lazy as BL
import           Data.Char (toLower)
import           Data.List(replicate, concat, take, foldl', elem, unwords, isInfixOf)
import           Data.Map.Internal(Map(..))
import qualified Data.Map.Strict as Map
import           Data.Set(Set)
import qualified Data.Set as Set
import           Data.Text(pack, Text)
import           Data.Vector.Unboxed(Vector)
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Storable.Mutable as SM
import           Data.Proxy(Proxy(..))
import           Foreign.ForeignPtr(withForeignPtr)
import           Foreign.Marshal.Array(peekArray)
import           GHC.Generics(Generic)
import qualified Graphics.UI.GLFW as GLFW
import           Numeric(showFFloat)
import           Options.Applicative(ReadM, str, option, long, help, readerError)
import qualified Sound.PortMidi as PortMidi
import           System.IO(withFile, IOMode(..))
import           System.Directory(doesFileExist)
import           System.FilePath(takeExtension, splitFileName, dropExtension)
import           Text.Read(readMaybe)

import           Imj.Arg.Class
import           Imj.Audio
import           Imj.Audio.Harmonics
import           Imj.Audio.Midi
import           Imj.Audio.SpaceResponse
import           Imj.Categorized
import           Imj.ClientView.Types
import           Imj.Data.AlmostFloat
import           Imj.Event
import           Imj.File
import           Imj.Game.App(runGame)
import           Imj.Game.Draw
import           Imj.Game.Class
import           Imj.Game.Command
import           Imj.Game.KeysMaps
import           Imj.Game.Modify
import           Imj.Game.Show
import           Imj.Game.State
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
import qualified Imj.Graphics.UI.Choice as UI
import           Imj.Music.Instruments
import           Imj.Music.Types
import           Imj.Music.PressedKeys
import           Imj.Music.Record
import           Imj.Server.Class hiding(Do)
import           Imj.Server.Connection
import           Imj.Server.Types
import           Imj.Server
import           Imj.Timing

main :: IO ()
main = runGame (Proxy :: Proxy SynthsGame)

-- from https://hackage.haskell.org/package/Euterpea-2.0.2/src/Euterpea/IO/MIDI/MidiIO.lhs
msgToMidi :: PortMidi.PMMsg -> Maybe Message
msgToMidi (PortMidi.PMMsg m d1 d2) =
  let k = (m .&. 0xF0) `shiftR` 4
      c = fromIntegral (m .&. 0x0F) -- channel
  in case k of
    0x8 -> Just $ NoteOff c (fromIntegral d1) (fromIntegral d2)
    0x9 -> Just $ NoteOn  c (fromIntegral d1) (fromIntegral d2)
    0xA -> Just $ KeyPressure c (fromIntegral d1) (fromIntegral d2)
    0xB -> Just $ ControlChange c (fromIntegral d1) (fromIntegral d2)
    0xC -> Just $ ProgramChange c (fromIntegral d1)
    0xD -> Just $ ChannelPressure c (fromIntegral d1)
    0xE -> Just $ PitchWheel c (fromIntegral (d1 + d2 `shiftL` 8))
    0xF -> Nothing -- SysEx event not handled
    _   -> Nothing

data LoopId = LoopId {
    _loopCreator :: {-# UNPACK #-} !ClientId
  , _loopIndex :: {-# UNPACK #-} !Int
} deriving(Generic, Show, Ord, Eq)
instance Binary LoopId
instance NFData LoopId


data EnvelopePart = EnvelopePart {
    _plot :: [MinMax Double]
  , _nSamples :: !Int
} deriving(Show)

widthPart :: EnvelopePart -> Int
widthPart = length . _plot

widthEnvelope :: Int
widthEnvelope = 90

findOscillations :: Instrument -> Maybe (Oscillator, Harmonics)
findOscillations (Wind _) = Nothing
findOscillations (Synth Noise _ _) = Nothing
findOscillations (Synth Sweep _ _) = Nothing
findOscillations (Synth (Oscillations osc har) _ _) = Just (osc, har)

toParts :: EnvelopeViewMode -> [Vector Double] -> [EnvelopePart]
toParts mode l@[ahds,r]
  | totalSamples == 0 = []
  | otherwise = map (uncurry mkMinMaxEnv) $ zip [widthAHDS, widthEnvelope - widthAHDS] l
 where
  mkMinMaxEnv w c =
    EnvelopePart
      (case mode of
        LogView -> resampleMinMaxLogarithmic (V.toList c) (V.length c) $ fromIntegral w
        LinearView -> resampleMinMaxLinear (V.toList c) (V.length c) $ fromIntegral w)
      $ V.length c
  ahdsSamples = V.length ahds
  rSamples = V.length r
  totalSamples = rSamples + ahdsSamples
  widthAHDS = round (fromIntegral widthEnvelope * fromIntegral ahdsSamples / fromIntegral totalSamples :: Float)
toParts _ _ = error "not supported"

data EnvelopePlot = EnvelopePlot {
    envParts :: [EnvelopePart]
  , envViewMode :: !EnvelopeViewMode
} deriving(Show)

data EnvelopeViewMode = LinearView | LogView
  deriving(Show)

toggleView :: EnvelopeViewMode -> EnvelopeViewMode
toggleView = \case
  LinearView -> LogView
  LogView -> LinearView

data EditMode =
    Envelope
  | Tone
  | Reverb
  deriving(Show, Eq)

data Edition = Edition {
    editMode :: !EditMode
  , envelopeIdx :: !EnvelopeParamIndex
  -- ^ Index of the enveloppe parameter that will be edited on left/right arrows.
  , harmonicIdx :: !Int
  -- ^ Index of the harmonic parameter that will be edited on left/right arrows.
  , reverbIdx_ :: !Int
  -- ^ Index of the reverb parameter that will be edited on left/right arrows.
} deriving(Show)

mkEdition :: Edition
mkEdition = Edition Envelope 0 0 0

toggleEditMode :: Edition -> Edition
toggleEditMode e = case editMode e of
  Envelope -> e {editMode = Tone}
  Tone -> e {editMode = Reverb}
  Reverb -> e {editMode = Envelope}

setEditionIndex :: Int -> Edition -> Edition
setEditionIndex idx (Edition mode i j k) = case mode of
  Envelope -> Edition mode (fromIntegral idx) j k
  Tone -> Edition mode i idx k
  Reverb -> Edition mode i j idx

getEditionIndex :: Edition -> Int
getEditionIndex (Edition mode i j k) =
  editiontIndex `mod` (countEditables mode)
 where
  editiontIndex = case mode of
    Envelope -> fromIntegral i
    Tone -> j
    Reverb -> k

  countEditables Envelope = 9
  countEditables Tone =
    -- 1 for sound source, 1 for oscillator
    1 + 1 + 2 * countHarmonics
  countEditables Reverb =
    -- 1 for by index, 1 for by size, 1 for reverb dry/wet
    3

widthEditMode :: Edition -> Length Width
widthEditMode (Edition mode _ _ _) = case mode of
  Envelope -> 15
  Tone -> 20
  Reverb -> 15

-- TODO use a Trie for allRevs
data Reverbs = Reverbs {
    wetRatio :: !AlmostFloat
  , allRevs :: Map FilePath SpaceResponse
  , indicesByAscendingDuration :: !(V.Vector Int)
  , commonPref :: !FilePath
  -- ^ This path is shared by all reverbs, it should be prefixed to keys of 'allRevs'
  -- to get the path of the reverb.
  , curRev :: !(Maybe Int)
} deriving(Show)

mkReverbs :: IO Reverbs
mkReverbs = do
  a <- allReverbs
  let pref = fromMaybe "" $ Map.foldlWithKey (\cpref n _ -> Just $ maybe n (commonPrefix n) cpref) Nothing a
      cprefLen = length pref
      !b = Map.fromList $ map (\(k,v) -> (drop cprefLen k, v)) $ Map.toList a
      byDuration = V.fromList $ indexesBy lengthInSeconds $ Map.elems b
  return $ Reverbs (almost 1) b byDuration pref Nothing

currentReverb :: Reverbs -> Maybe (FilePath, SpaceResponse)
currentReverb (Reverbs _ l _ pref c) =
  maybe Nothing (\idx -> if idx >= Map.size l
    then
      Nothing
    else
      Just $ addPref $ Map.elemAt idx l) c
 where
  addPref (k,v) = (pref ++ k, v)

data SynthsGame = SynthsGame {
    pianos :: !(Map ClientId (PressedKeys InstrumentId))
  , pianoLoops :: !(Map SequencerId (Map LoopId (PressedKeys InstrumentId)))
  , clientPressedKeys :: !(Map Key (InstrumentNote InstrumentId))
  , instrument :: !(Maybe (InstrumentId,Instrument))
  , envelopePlot :: !EnvelopePlot
  , mayLastOscillations_ :: !(Maybe SoundSource)
  -- ^ We store the last used 'Oscillation' to restore it when needed
  , edition :: !Edition
  , midiSourceIdx :: !(Maybe MidiSourceIdx)
  , reverbs :: !Reverbs
} deriving(Show)

instance UIInstructions SynthsGame where
  instructions color@(LayeredColor bg fg) (SynthsGame _ _ _ mayInstr _ _ edit@(Edition mode _ _ _) _ rev) = maybe [] (\(_,instr) -> case instr of
    Synth oscs release (AHDSR'Envelope a h d r ai di ri s) -> case mode of
      Envelope -> envelopeInstructions
      Tone -> harmonicsInstructions
      Reverb -> reverbInstructions

     where

      envelopeInstructions =
        [ ConfigUI "Attack"
            [ mkChoice 0 $ show a
            , mkChoice 1 $ show ai
            ]
        , ConfigUI "Hold"
            [ mkChoice 2 $ show h]
        , ConfigUI "Decay"
            [ mkChoice 3 $ show d
            , mkChoice 4 $ show di
            ]
        , ConfigUI "Sustain"
            [ mkChoice 5 $ show s
            ]
        , ConfigUI "Release"
            [ mkChoice 6 $ show r
            , mkChoice 7 $ show ri
            ]
        , ConfigUI "Auto-release"
            [ mkChoice 8 $ case release of
                AutoRelease -> "Yes"
                KeyRelease -> "No"
            ]
        ]

      harmonicsInstructions = case oscs of
        Oscillations osc harmonics ->
          [ ConfigUI "Source"
              [mkChoice sourceIdx $ "Oscillators"]
          , ConfigUI "Oscillator"
              [ mkChoice oscillatorIdx $ show osc ]
          , hInst "Harmo. vol." volume firstHarVolIdx
          , hInst "Harmo. ang." phase firstHarPhaseIdx
          ]
         where
           hInst title f startIdx = ConfigUI title $
            map
             (\(i,har) -> mkChoice i $ show $ f har)
             (zip [startIdx..] $ S.toList $ unHarmonics harmonics)
        Noise ->
          [ ConfigUI "Source"
              [mkChoice sourceIdx $ "Noise"]
          , ConfigUI "Oscillator" []
          , ConfigUI "Harmo. vol." []
          , ConfigUI "Harmo. ang." []
          ]
        Sweep ->
          [ ConfigUI "Source"
              [mkChoice sourceIdx $ "Sweep"]
          , ConfigUI "Oscillator" []
          , ConfigUI "Harmo. vol." []
          , ConfigUI "Harmo. ang." []
          ]

      reverbInstructions =
        [ ConfigUI "Reverb info" $ maybe [List color ["None"]] (\(pathRev, i) ->
          let (dirName, fileName) = splitFileName $ dropExtension pathRev
          in [ --List (LayeredColor bg $ dim 2 fg) [pack $ commonPref rev],
               List color [pack fileName]
             , List (LayeredColor bg $ dim 2 fg) [pack $ drop (length $ commonPref rev) dirName]
             -- we dim the foreground color to indicate that it's not editable.
             , List (LayeredColor bg $ dim 2 fg) $ map pack $
                   [ showDur i
                   , (show $ countChannels i) ++ " channels"
                   ]
             ]) $ currentReverb rev
        , ConfigUI "By index" $
            [ mkChoice reverbBySizeIdx $ unwords [ maybe
                "0"
                (\n -> show $ n+1)
                $ curRev rev
                , "of"
                , show $ Map.size $ allRevs rev ]
            ]
        , ConfigUI "By duration" $
            [ mkChoice reverbByDurationIdx $ maybe
                "0.00 s"
                (\(_, i) -> showDur i)
                $ currentReverb rev
            ]
        , ConfigUI "Wet ratio"
            [ mkChoice reverbWetIdx $ show $ wetRatio rev]
        ]
       where
        showDur x = (showFFloat (Just 2) (lengthInSeconds x) "") ++ " s"

      mkChoice x v =
        Choice $ UI.Choice (pack v) right left color


       where

        right
          | x == idx = '>'
          | otherwise = ' '
        left
          | x == idx = '<'
          | otherwise = ' '
        idx = getEditionIndex edit

    _ -> []) mayInstr


data Key =
    GLFWKey !GLFW.Key
    -- ^ A key pressed from the PC-keyboard
  | MIDIKey {-# UNPACK #-} !MidiPitch -- TODO include channel once we handle several midi channels
    -- ^ A key pressed from a MIDI device
 deriving(Show, Ord, Eq)

defaultSynth :: Instrument
defaultSynth = organicInstrument

countHarmonics :: Int
countHarmonics = 10

sourceIdx, oscillatorIdx, firstHarVolIdx, firstHarPhaseIdx, reverbBySizeIdx, reverbByDurationIdx, reverbWetIdx :: Int
sourceIdx = 0
oscillatorIdx = sourceIdx + 1
firstHarVolIdx = oscillatorIdx + 1
firstHarPhaseIdx = firstHarVolIdx + countHarmonics

reverbBySizeIdx = 0
reverbByDurationIdx = 1
reverbWetIdx = 2

predefinedAttackItp, predefinedDecayItp, predefinedReleaseItp :: Set Interpolation
predefinedDecayItp = allDifferentiableInterpolations
predefinedAttackItp = Set.delete ProportionaValueDerivative allDifferentiableInterpolations
predefinedReleaseItp = predefinedAttackItp

predefinedHarmonicsVolumes :: Set AlmostFloat
predefinedHarmonicsVolumes = Set.fromList [0, 0.01, 0.1, 1]

predefinedHarmonicsPhases :: Set AlmostFloat
predefinedHarmonicsPhases = Set.fromList $ takeWhile (< 2) $ map ((*) 0.1 . fromIntegral) [0::Int ..]

predefinedWetRatios :: Set AlmostFloat
predefinedWetRatios = Set.fromList $ map (flip (/) 10 . fromIntegral) [0::Int ..10]

predefinedAttack, predefinedHolds, predefinedDecays, predefinedReleases :: Set Int
predefinedSustains :: Set AlmostFloat
predefinedAttack =
  let l = 50:map (*2) l
  in Set.fromDistinctAscList $ take 12 l
predefinedHolds =
  let l = 5:map (*2) l
  in Set.fromDistinctAscList $ 0:take 12 l
predefinedDecays = predefinedAttack
predefinedReleases = predefinedAttack
predefinedSustains =
  let l = 0.01:map (*1.3) l
  in Set.fromDistinctAscList $ 0:takeWhile (< 1) l ++ [1]

withMinimumHarmonicsCount :: Instrument -> Instrument
withMinimumHarmonicsCount i@(Wind _) = i
withMinimumHarmonicsCount i@(Synth src _ _) = case src of
  Noise -> i
  Sweep -> i
  Oscillations osc hars ->
    let prevH = unHarmonics hars
    in i { source_ = Oscillations osc $ Harmonics $
            prevH S.++
            (S.fromList $ replicate (countHarmonics - S.length prevH) (HarmonicProperties 0 0))
          }

allReverbs :: IO (Map FilePath SpaceResponse)
allReverbs = do
  let extensions = [".wav",".wir"]
  paths <- filter (flip elem extensions . map toLower . takeExtension) <$> listFilesRecursively "../audio.ir"
  Map.fromList . catMaybes <$> mapM
    (\p -> fmap ((,) p) <$> uncurry getReverbInfo (splitFileName p))
    paths

initialGame :: IO SynthsGame
initialGame =
  SynthsGame mempty mempty mempty Nothing (EnvelopePlot [] LogView) Nothing mkEdition Nothing <$> mkReverbs

data SynthsMode =
    PlaySynth
  | EditSynth
  deriving(Show)

data SynthsServer = SynthsServer {
    srvSequencers :: !(Map SequencerId (MVar (Sequencer LoopId InstrumentId)))
  , nextSeqId :: !SequencerId
  , nextMIDISourceIdx :: !MidiSourceIdx
} deriving(Generic)
instance NFData SynthsServer

data SynthsClientView = SynthsClientView {
    _piano :: !(PressedKeys InstrumentId)
    -- ^ What is currently pressed, by the player (excluding what is played by loops)
  , _recording :: !(Recording InstrumentId)
    -- ^ The ongoing recording of what is being played, which can be used to create a 'Loop'
  , _nextLoopPianoIdx :: !Int
} deriving(Generic, Show)
instance NFData SynthsClientView

thePianoValue :: SynthsClientView -> PressedKeys InstrumentId
thePianoValue (SynthsClientView x _ _) = x

-- | The server will also send 'PlayMusic' events, which are generic events.
data SynthsServerEvent =
    PianoValue !(Either ClientId (SequencerId, LoopId)) !(PressedKeys InstrumentId)
  | NewLine {-# UNPACK #-} !SequencerId {-# UNPACK #-} !LoopId
  | AssignedSourceIdx {-# UNPACK #-} !MidiSourceIdx
  deriving(Generic, Show)
instance DrawGroupMember SynthsServerEvent where
  exclusivityKeys = \case
    AssignedSourceIdx {} -> mempty
    NewLine {} -> mempty
    PianoValue {} -> mempty -- we do this is so that interleaving 'PianoValue' events with 'PlayMusic' events
                           -- will still allow multiple play music events to be part of the same frame.
                           -- It is a bit of a hack, the better solution would be to handle audio events
                           -- client-side as soon as they are received (in the listening thread) (TODO).

instance Categorized SynthsServerEvent
instance NFData SynthsServerEvent
instance Binary SynthsServerEvent

-- TODO the global client instrument Id should be set to Nothing when the instrument is changed.
-- The maybe we should see if there is a correponding instrument in the map,
-- and if yes return the existing 'InstrumentId', else create a new 'InstrumentId'.
-- To avoid overhead when sending a Note, we should do this work at each instrument
-- parameter change.
data SynthClientEvent =
    PlayNote !(MusicalEvent InstrumentId)
  | WithMultiLineSequencer {-# UNPACK #-} !SequencerId
  | WithMonoLineSequencer
  | ForgetCurrentRecording
  deriving(Show,Generic)
instance Binary SynthClientEvent
instance Categorized SynthClientEvent

data ReverbChangeType =
    ByIndex
  | ByDuration
  deriving(Show)

data SynthsGameEvent =
    ChangeInstrument !Instrument
  | ChangeReverb {-# UNPACK #-} !Int !ReverbChangeType
  | ChangeReverbWet {-# UNPACK #-} !Int
  | ChangeEditedFeature {-# UNPACK #-} !Int
  | ToggleEditMode
  | ToggleEnvelopeViewMode
  | InsertPressedKey !Key !(InstrumentNote InstrumentId)
  | RemovePressedKey !Key
  deriving(Show)
instance Categorized SynthsGameEvent
instance DrawGroupMember SynthsGameEvent where
  exclusivityKeys _ = mempty

instance GameExternalUI SynthsGame where

  gameWindowTitle = const "Play some music!"

  -- NOTE 'getViewport' is never called unless 'putIGame' was called once.
  getViewport _ (Screen _ center) SynthsGame{} =
    mkCenteredRectContainer center $ Size 45 100

changeInstrumentValue :: Instrument -> Maybe SoundSource -> Edition -> Int -> Instrument
changeInstrumentValue instr mayLastOscillations edit@(Edition mode _ _ _) inc =
  case instr of
    synth@(Synth src _ _) ->
      case mode of
        Envelope ->
          changeInstrumentEnvelopeIndexedValue instr (fromIntegral idx) inc
        Tone ->
          if idx == sourceIdx then synth {
              source_ = case src of
                Sweep -> Noise
                Noise -> maybe (source_ defaultSynth) id mayLastOscillations
                Oscillations _ _ -> Sweep
            }
          else if idx == oscillatorIdx then case src of
            Sweep -> error "UI consistency : Sweep has no oscillator"
            Noise -> error "UI consistency : Noise has no oscillator"
            Oscillations osc har -> synth { source_ = Oscillations (cycleOscillator inc osc) har }
          else changeInstrumentHarmonic (idx-(oscillatorIdx+1)) instr inc
        Reverb -> error "logic"
    _ -> instr
 where
  idx = getEditionIndex edit

controlToIndex :: Int -> Maybe EnvelopeParamIndex
controlToIndex = \case
  102 -> Just 0
  103 -> Just 1
  104 -> Just 2
  106 -> Just 3
  107 -> Just 4
  108 -> Just 5
  110 -> Just 6
  111 -> Just 7
  112 -> Just 8
  _ -> Nothing

newtype EnvelopeParamIndex = EnvelopeParamIndex Int
  deriving (Num, Eq, Show, Integral, Real, Ord, Enum)

changeInstrumentEnvelopeIndexedValue :: Instrument -> EnvelopeParamIndex -> Int -> Instrument
changeInstrumentEnvelopeIndexedValue i@(Wind _) _ _ = i
changeInstrumentEnvelopeIndexedValue instr@(Synth _ release p@(AHDSR'Envelope a h d r ai di ri s)) idx inc =
  case idx of
    0 -> instr { envelope_ = p {ahdsrAttack = changeParam predefinedAttack a inc} }
    1 -> instr { envelope_ = p {ahdsrAttackItp = changeParam predefinedAttackItp ai inc} }
    2 -> instr { envelope_ = p {ahdsrHold = changeParam predefinedHolds h inc} }
    3 -> instr { envelope_ = p {ahdsrDecay = changeParam predefinedDecays d inc} }
    4 -> instr { envelope_ = p {ahdsrDecayItp = changeParam predefinedDecayItp di inc} }
    5 -> instr { envelope_ = p {ahdsrSustain = changeParam predefinedSustains s inc} }
    6 -> instr { envelope_ = p {ahdsrRelease = changeParam predefinedReleases r inc} }
    7 -> instr { envelope_ = p {ahdsrReleaseItp = changeParam predefinedReleaseItp ri inc} }
    8 -> instr { releaseMode_ = cycleReleaseMode release }
    _ -> error "logic"

changeInstrumentHarmonic :: Int -> Instrument -> Int -> Instrument
changeInstrumentHarmonic _ instr@(Wind _ ) _ = instr
changeInstrumentHarmonic _ instr@(Synth Noise _ _) _ = instr
changeInstrumentHarmonic _ instr@(Synth Sweep _ _) _ = instr
changeInstrumentHarmonic idx instr@(Synth (Oscillations osc (Harmonics harmonics)) _ _) inc =
  instr { source_ = Oscillations osc $ Harmonics $ h' S.// [(idx', newVal)] }
 where
  idx'
   | idx >= countHarmonics = idx - countHarmonics
   | otherwise = idx

  h'
   | S.length harmonics <= idx' =
      harmonics S.++ (S.fromList $ replicate (1 + idx' - S.length harmonics) (HarmonicProperties 0 0))
   | otherwise = harmonics

  oldVal = S.unsafeIndex h' idx'

  newVal
    | idx >= countHarmonics =
        oldVal { phase = changeParam predefinedHarmonicsPhases (phase oldVal) inc }
    | otherwise =
        oldVal { volume = changeParam predefinedHarmonicsVolumes (volume oldVal) inc }

changeInstrumentOrReverb :: Maybe Instrument -> Maybe SoundSource -> Edition -> Int -> Maybe (Event SynthsGameEvent)
changeInstrumentOrReverb mayInstr mayLastOscillations edit inc
  | editMode edit == Reverb =
     let ev
          | idx == reverbBySizeIdx     = flip ChangeReverb ByIndex
          | idx == reverbByDurationIdx = flip ChangeReverb ByDuration
          | idx == reverbWetIdx        = ChangeReverbWet
          | otherwise = error "logic"
     in Just $ AppEvent $ ev inc
  | otherwise =
      maybe Nothing (\instr -> case instr of
        Synth{} -> Just $ AppEvent $ ChangeInstrument $ changeInstrumentValue instr mayLastOscillations edit inc
        _ -> Nothing) mayInstr
 where
  idx = getEditionIndex edit

data SynthsStatefullKeys
instance GameStatefullKeys SynthsGame SynthsStatefullKeys where

  mapStateKey _ GLFW.Key'Space GLFW.KeyState'Pressed _ _ _ =
    return [CliEvt $ ClientAppEvt WithMonoLineSequencer]
  mapStateKey _ GLFW.Key'F1 GLFW.KeyState'Pressed _ _ _ =
    return [CliEvt $ ClientAppEvt $ WithMultiLineSequencer $ SequencerId 1]
  mapStateKey _ GLFW.Key'F2 GLFW.KeyState'Pressed _ _ _ =
    return [CliEvt $ ClientAppEvt $ WithMultiLineSequencer $ SequencerId 2]
  mapStateKey _ GLFW.Key'F3 GLFW.KeyState'Pressed _ _ _ =
    return [CliEvt $ ClientAppEvt $ WithMultiLineSequencer $ SequencerId 3]
  mapStateKey _ GLFW.Key'F4 GLFW.KeyState'Pressed _ _ _ =
    return [CliEvt $ ClientAppEvt $ WithMultiLineSequencer $ SequencerId 4]
  mapStateKey _ GLFW.Key'F5 GLFW.KeyState'Pressed _ _ _ =
    return [Evt $ AppEvent $ ToggleEditMode]
  mapStateKey _ GLFW.Key'F6 GLFW.KeyState'Pressed _ _ _ =
    return [Evt $ AppEvent $ ToggleEnvelopeViewMode]
  mapStateKey _ GLFW.Key'F10 GLFW.KeyState'Pressed _ _ _ =
    return [CliEvt $ ClientAppEvt ForgetCurrentRecording]
  mapStateKey _ k st m _ g = return $ maybe
    []
    (\(SynthsGame _ _ pressed mayInstr _ mayLastOsc edit _ _) -> maybe
      (case st of
        GLFW.KeyState'Repeating -> []
        GLFW.KeyState'Pressed -> maybe
            []
            (\noteSpec ->
              maybe
                []
                (\(iid, _) ->
                  let spec = noteSpec iid
                  in [CliEvt $ ClientAppEvt $ PlayNote $ StartNote Nothing spec 1
                    , Evt $ AppEvent $ InsertPressedKey (GLFWKey k) spec])
                mayInstr)
            $ keyToNote k
        GLFW.KeyState'Released -> maybe
            []
            (\spec ->
               [CliEvt $ ClientAppEvt $ PlayNote $ StopNote Nothing spec
              , Evt $ AppEvent $ RemovePressedKey $ GLFWKey k])
            $ Map.lookup (GLFWKey k) pressed)
      (\dir -> do

          let amount
               | m == altOnly = 10
               | otherwise = 1
              mayInc = case dir of
               RIGHT -> Just amount
               LEFT  -> Just $ -amount
               _ -> Nothing
          case st of
            GLFW.KeyState'Released -> []
            -- pressed or repeating:
            _ ->
              maybe
                (maybe [] (\(_, instr) -> case instr of
                  Synth{} -> case dir of
                      Up   -> [Evt $ AppEvent $ ChangeEditedFeature $ idx - 1]
                      Down -> [Evt $ AppEvent $ ChangeEditedFeature $ idx + 1]
                      _ -> error "logic"
                     where
                      idx = getEditionIndex edit
                  _ -> []) mayInstr)
                (map Evt . maybeToList . changeInstrumentOrReverb (fmap snd mayInstr) mayLastOsc edit)
                mayInc)
        $ isArrow k)
    $ _game $ getGameState' g

   where

    keyToNote = \case
      -- NOTE GLFW uses the US keyboard layout to name keys: https://en.wikipedia.org/wiki/British_and_American_keyboards
      -- lower keys
      GLFW.Key'Z -> Just $ InstrumentNote Do $ noOctave - 1
      GLFW.Key'S -> Just $ InstrumentNote Réb $ noOctave - 1
      GLFW.Key'X -> Just $ InstrumentNote Ré $ noOctave - 1
      GLFW.Key'D -> Just $ InstrumentNote Mib $ noOctave - 1
      GLFW.Key'C -> Just $ InstrumentNote Mi $ noOctave - 1
      GLFW.Key'V -> Just $ InstrumentNote Fa $ noOctave - 1
      GLFW.Key'G -> Just $ InstrumentNote Solb $ noOctave - 1
      GLFW.Key'B -> Just $ InstrumentNote Sol $ noOctave - 1
      GLFW.Key'H -> Just $ InstrumentNote Lab $ noOctave - 1
      GLFW.Key'N -> Just $ InstrumentNote La $ noOctave - 1
      GLFW.Key'J -> Just $ InstrumentNote Sib $ noOctave - 1
      GLFW.Key'M -> Just $ InstrumentNote Si $ noOctave - 1
      GLFW.Key'Comma -> Just $ InstrumentNote Do $ noOctave + 0
      GLFW.Key'L -> Just $ InstrumentNote Réb $ noOctave + 0
      GLFW.Key'Period -> Just $ InstrumentNote Ré $ noOctave + 0
      GLFW.Key'Semicolon -> Just $ InstrumentNote Mib $ noOctave + 0
      GLFW.Key'Slash -> Just $ InstrumentNote Mi $ noOctave + 0
      -- upper keys
      GLFW.Key'Q -> Just $ InstrumentNote Do $ noOctave + 0
      GLFW.Key'2 -> Just $ InstrumentNote Réb $ noOctave + 0
      GLFW.Key'W -> Just $ InstrumentNote Ré $ noOctave + 0
      GLFW.Key'3 -> Just $ InstrumentNote Mib $ noOctave + 0
      GLFW.Key'E -> Just $ InstrumentNote Mi $ noOctave + 0
      GLFW.Key'R -> Just $ InstrumentNote Fa $ noOctave + 0
      GLFW.Key'5 -> Just $ InstrumentNote Solb $ noOctave + 0
      GLFW.Key'T -> Just $ InstrumentNote Sol $ noOctave + 0
      GLFW.Key'6 -> Just $ InstrumentNote Lab $ noOctave + 0
      GLFW.Key'Y -> Just $ InstrumentNote La $ noOctave + 0
      GLFW.Key'7 -> Just $ InstrumentNote Sib $ noOctave + 0
      GLFW.Key'U -> Just $ InstrumentNote Si $ noOctave + 0
      GLFW.Key'I -> Just $ InstrumentNote Do $ noOctave + 1
      GLFW.Key'9 -> Just $ InstrumentNote Réb $ noOctave + 1
      GLFW.Key'O -> Just $ InstrumentNote Ré $ noOctave + 1
      GLFW.Key'0 -> Just $ InstrumentNote Mib $ noOctave + 1
      GLFW.Key'P -> Just $ InstrumentNote Mi $ noOctave + 1
      GLFW.Key'LeftBracket -> Just $ InstrumentNote Fa $ noOctave + 1
      GLFW.Key'Equal -> Just $ InstrumentNote Solb $ noOctave + 1
      GLFW.Key'RightBracket -> Just $ InstrumentNote Sol $ noOctave + 1
      _ -> Nothing

-- ctrl has a bug on OSX / GLFW : Ctrl + arrow left events are not received
altOnly :: GLFW.ModifierKeys
altOnly = GLFW.ModifierKeys {
    GLFW.modifierKeysShift   = False
  , GLFW.modifierKeysControl = False
  , GLFW.modifierKeysAlt     = True
  , GLFW.modifierKeysSuper   = False
  }

changeParam :: (Ord a) => Set a -> a -> Int -> a
changeParam predefined current direction
  | direction < 0 = go Set.lookupLT (-direction)
  | otherwise     = go Set.lookupGT direction
 where
  go lookupF =
    flip go' current
   where
    go' 0 cur = cur
    go' n cur =
      maybe cur (go' (n-1)) $ lookupF cur predefined

instrumentFile :: FilePath
instrumentFile = "instruments/last.inst"

loadInstrument :: IO Instrument
loadInstrument = doesFileExist instrumentFile >>= bool
  (return defaultSynth)
  (do
    bl <- BL.readFile instrumentFile
    let len = BL.length bl
    either
      (\(_,offset,msg) -> fail $ "The file '" ++ instrumentFile ++ "' is corrupt:" ++ show (offset,msg))
      (\(_,offset,res :: Instrument) ->
        if fromIntegral len == offset
          then
            return res
          else
            fail $ "Not all content has been used :" ++ show (len,offset) ) $
      (decodeOrFail bl))

saveInstrument :: Instrument -> IO ()
saveInstrument i = do
  createDirectories instrumentFile
  withFile instrumentFile WriteMode $ \h ->
    BL.hPutStr h (encode i)

data MidiDeviceContext = MidiDeviceContext {
    midiStream :: {-# UNPACK #-} !PortMidi.PMStream
  , pollPeriod :: {-# UNPACK #-} !MIDIPollPeriod
  , unsafeStorage :: !(S.Vector PortMidi.PMEvent)
  -- ^ This buffer is used as static storage when reading midi events
  -- from the c library @portmidi@. It may contain uninitialized / invalid elements.
}

mkMidiDeviceContext :: PortMidi.PMStream -> MIDIPollPeriod -> MidiDeviceContext
mkMidiDeviceContext s p = MidiDeviceContext s p $ S.create $ SM.new 256


-- | The period, in microseconds, at which the midi events are polled.
newtype MIDIPollPeriod = MIDIPollPeriod { unMIDIPollPeriod :: Int64 }
  deriving(Num, Integral, Real, Enum, Show, Eq, Ord)
defaultMidiPollPeriod :: MIDIPollPeriod
defaultMidiPollPeriod = 10000 -- a sane CPU usage / latency ratio.

data SynthsClientArgs = SynthsClientArgs {
    midiPollPeriod :: {-# UNPACK #-} !MIDIPollPeriod
} deriving(Show)
instance Arg SynthsClientArgs where
  parseArg = Just $ (SynthsClientArgs <$>
      option midiPollPeriodArg
         (  long "midiPollPeriod"
         <> help (
         "[Client MIDI] The period, in microseconds, of MIDI events polling. " ++
         "Smaller poll periods reduce MIDI jitter and latency, but require more CPU cycles. " ++
         "The default period (\"" ++ show (unMIDIPollPeriod defaultMidiPollPeriod) ++
         "\") makes MIDI latency and jitter almost unnoticeable " ++
         "and is easy on the CPU."
         )))


midiPollPeriodArg :: ReadM MIDIPollPeriod
midiPollPeriodArg =
  str >>= maybe
    (readerError "Encountered an unreadable MIDI poll period. This should be a positive number")
    (\(i :: Int64) ->
      if i < 0
        then
          readerError "Encountered a negative MIDI poll period."
        else
          return $ fromIntegral i)
    . readMaybe

-- | We filter devices that contain "through" in their name (these are fake devices on linux).
--
-- You may need to tweak this function if you have mutliple midi devices to chose from,
-- and the one selected is the wrong one.
selectDevice :: [(a, PortMidi.DeviceInfo)]
             -> Maybe (a, PortMidi.DeviceInfo)
selectDevice inputDevices =
  let inputDevicesNotThrough = filter (not . isInfixOf "through" . map toLower . PortMidi.name . snd) inputDevices
  in listToMaybe inputDevicesNotThrough

instance GameLogic SynthsGame where

  type ClientArgsT SynthsGame = SynthsClientArgs
  type ServerT SynthsGame = SynthsServer
  type StatefullKeysT SynthsGame = SynthsStatefullKeys
  type ClientOnlyEvtT SynthsGame = SynthsGameEvent
  type PollContextT SynthsGame = MidiDeviceContext

  produceEventsByPolling = EventProducerByPolling {
    -- Never returns 'Left', to let the app run in a degraded mode (pc-keyboard only)
    --   when midi input is not available
    initializeProducer = \mayClientArg -> do
      let period = fromMaybe defaultMidiPollPeriod $ fmap midiPollPeriod mayClientArg
      putStrLn "PortMidi initializes."
      PortMidi.initialize >>= either
        (\err -> do
          putStrLn $ "PortMidi initialization failed:" ++ show err
          putStrLn "PortMidi: You will be able to play sounds using your pc-keyboard only."
          PortMidi.terminate >>= either (putStrLn . (++) "PortMidi termination failed:" . show) (const $ return ())
          return $ Right Nothing)
        (\_ -> do
          PortMidi.countDevices >>= \n -> do
            putStrLn $ unwords [show n, "MIDI devices detected."]
            devices <- mapM (\i -> (,) i <$> PortMidi.getDeviceInfo i) [0..n-1]
            mapM_ print devices
            let mayInputDevice = selectDevice $ filter (PortMidi.input . snd) devices
            maybe
                (do
                  putStrLn "PortMidi: No valid input midi device exists."
                  putStrLn "PortMidi: You will be able to play sounds using your pc-keyboard only."
                  PortMidi.terminate >>= either (putStrLn . (++) "PortMidi termination failed:" . show) (const $ return ())
                  return $ Right Nothing)
                (\(did, inputDevice) -> do
                     putStrLn $ (++) "Chosen MIDI device:" $ show inputDevice
                     putStrLn "PortMidi is opening the MIDI device."
                     PortMidi.openInput did >>= either
                        (\err -> do
                          putStrLn $ "PortMidi: Failed to open the device:" ++ show err
                          putStrLn "PortMidi: You will be able to play sounds using your pc-keyboard only."
                          PortMidi.terminate >>= either (putStrLn . (++) "PortMidi termination failed:" . show) (const $ return ())
                          return $ Right Nothing)
                        (\res -> do
                          putStrLn "PortMidi: opened the device."
                          return $ Right $ Just $ mkMidiDeviceContext res period))
                 mayInputDevice)
    , produceEvents = \(MidiDeviceContext stream period buffer) mayGame -> do
      -- 100 microseconds is the minimal time between calls (measured using console prints).
      -- but this is achieved only by setting a lower value like so:
      let dt = fromMicros $ fromIntegral period
      PortMidi.poll stream >>= either
        (return . Left . pack . (++) "midi poll:" . show)
        (\case
            PortMidi.NoError'NoData ->
              return $ Right ([],[],Just dt)
            PortMidi.GotData -> do
              let (bufferPtr, bufferSz) = S.unsafeToForeignPtr0 buffer
                  go = withForeignPtr bufferPtr $ \ptr -> do
                        res <- PortMidi.readEventsToBuffer stream ptr $ fromIntegral bufferSz
                        nReads <- fromIntegral <$> either
                          (\err -> do
                            putStrLn $ "midi read error:" ++ show err
                            return 0)
                          return
                          res
                        (l1,l2) <- maybe
                          -- in that case, we don't use the events we just read,
                          -- but reading them serves the purpose of not overflowing the queue.
                          (return ([],[]))
                          (\(SynthsGame _ _ pressed mayInstr _ mayLastOsc edit maySourceIdx _) -> maybe
                            (return ([],[])) -- should not happen once connected.
                            (\srcIdx -> -- maybe () (\(iid, instr) -> ...) mayInstr
                              (foldl' (\(a,b) (c,d) -> (a++c, b++d)) ([],[]).
                                  map
                                    (\(PortMidi.PMEvent msg time) ->
                                    -- Portmidi time is in milliseconds, we convert it to nanoseconds.
                                    let mi = Just $ MidiInfo (fromIntegral $ time * 1000000) srcIdx
                                        off k =
                                          let key = MIDIKey k
                                              mayPressedSpec = Map.lookup key pressed
                                              -- 'pressed' might not be up-to-date if we have a noteon and a note off in the same poll period.
                                              mayFallbackSpec = fmap
                                                (\(iid, _) -> mkInstrumentNote k iid)
                                                mayInstr
                                              -- TODO we assume that the current instrument will not
                                              -- change during the poll period. A more robust solution would be
                                              -- to modify 'pressed' during 'produceEvents' instead of relying on
                                              -- sending events, after the call. Then we won't need mayFallbackSpec.
                                              maySpec = mayPressedSpec <|> mayFallbackSpec
                                          in maybe
                                              ([],[])
                                              (\spec ->
                                                ([AppEvent $ RemovePressedKey key]
                                                ,[ClientAppEvt $ PlayNote $ StopNote mi spec]))
                                              maySpec
                                        on k v = maybe
                                          ([],[])
                                          (\(iid, _) ->
                                           let i = mkInstrumentNote k iid
                                           in ([AppEvent $ InsertPressedKey (MIDIKey k) i]
                                             , [ClientAppEvt $ PlayNote $ StartNote mi i $ mkNoteVelocity v]))
                                          mayInstr
                                        ctrl control value
                                          | control == 12 =
                                              (AppEvent . ChangeEditedFeature . (+ (getEditionIndex edit))) <$>
                                                relativeValue (Just 1) value
                                          | control == 13 = maybe Nothing
                                              (changeInstrumentOrReverb (fmap snd mayInstr) mayLastOsc edit)
                                              $ relativeValue (Just 1) value
                                          | otherwise = maybe Nothing (ctrl' . snd) mayInstr
                                         where
                                          ctrl' instr
                                            | control == 24 = maybe Nothing
                                                (\(_, har) -> Just $ AppEvent $ ChangeInstrument $ instr { source_ = Oscillations (toEnum $ value `mod` countOscillators) har })
                                                $ findOscillations instr
                                            | control >= 14 && control <= 23 =
                                                (AppEvent . ChangeInstrument . changeInstrumentHarmonic (control - 14) instr) <$>
                                                  relativeValue (Just 1) value
                                            | control >= 52 && control <= 61 =
                                                (AppEvent . ChangeInstrument . changeInstrumentHarmonic (countHarmonics + control - 52) instr) <$>
                                                  relativeValue (Just 1) value
                                            | otherwise =
                                                maybe
                                                  Nothing
                                                  (\i ->
                                                    (AppEvent . ChangeInstrument . changeInstrumentEnvelopeIndexedValue instr i) <$>
                                                      relativeValue (Just 1) value) $
                                                  controlToIndex control

                                          relativeValue _ 64 = Nothing
                                          relativeValue mayMaxChange x
                                            | x > 64 = Just $ min maxChange $ x-64
                                            | otherwise = Just $ negate $ min maxChange $ (64-x)
                                            where maxChange = fromMaybe maxBound mayMaxChange
                                    in maybe
                                        ([],[])
                                        (\m -> {-traceShow (time,m) $ -} case m of
                                          NoteOff _ key _ -> off $ fromIntegral key
                                          NoteOn _ key 0 -> off $ fromIntegral key
                                          NoteOn _ key vel -> on (fromIntegral key) vel
                                          ControlChange _ control value ->
                                            maybe
                                              ([],[])
                                              (flip (,) [] . (:[]))
                                              $ ctrl control value
                                          _ -> ([],[]))
                                        $ msgToMidi $ PortMidi.decodeMsg msg)
                                      <$> flip peekArray ptr nReads))
                                      maySourceIdx) mayGame
                        return (l1,l2,nReads)
                  go' a b = do
                    -- read until there is nothing to read.
                    (l1,l2,nEvtsRead) <- go
                    let newA = a ++ l1
                        newB = b ++ l2
                    if nEvtsRead == bufferSz
                      then
                        go' newA {-$ trace "another read is needed"-} newB
                      else
                        return (newA, newB)
              (l1,l2) <- go' [] []
              return $ Right (l1,l2,Just dt))
    , terminateProducer =
        const $ PortMidi.terminate >>= either (return . Left . pack . show) (const $ return $ Right ())
    }

  mapInterpretedKey _ _ _ = return []

  onClientOnlyEvent e = do
    mayNewEnvMinMaxs <-
      getIGame >>= maybe (liftIO initialGame) return >>= \(SynthsGame _ _ _ mayInstr (EnvelopePlot _ viewmode) _ _ _ _) -> do
        case e of
          ChangeInstrument i -> do
            liftIO $ saveInstrument i
            -- we could omit this for harmonic changes:
            Just . toParts viewmode <$> liftIO (envelopeShape i)
          ToggleEnvelopeViewMode ->
            maybe
              (return Nothing)
              (\(_, instr) -> Just . toParts (toggleView viewmode) <$> liftIO (envelopeShape instr))
              mayInstr
          _ -> return Nothing
    getIGame >>= maybe (liftIO initialGame) return >>= \g@(SynthsGame _ _ pressed mayInstr _ mayLastOscs _ maySourceIdx rvbs@(Reverbs wet revs indexesByDur cpref mayCurIndex)) -> withAnim $ case e of
      ChangeInstrument i -> do
        useInstrument maySourceIdx i >>= maybe (return ()) (\instrId ->
          putIGame g {
              mayLastOscillations_ = maybe mayLastOscs
                (maybe mayLastOscs (Just . uncurry Oscillations) . findOscillations . snd)
                mayInstr
            , instrument = Just (instrId, i)
            , envelopePlot = EnvelopePlot (fromMaybe (error "logic") mayNewEnvMinMaxs) $ envViewMode $ envelopePlot g
            })
      ChangeReverb i changeType -> do
        let nullIndex = Map.size revs
            mayNewIndex
              | nullIndex == 0 = Nothing
              | otherwise = case changeType of
                ByIndex ->
                  let j = (i + (fromMaybe nullIndex mayCurIndex)) `mod` (nullIndex + 1)
                  in if j >= nullIndex || j < 0
                      then
                        Nothing
                      else
                        Just j
                ByDuration ->
                  let newVecIdx = i + maybe
                        (if i > 0 then -1 else nullIndex)
                        (fromMaybe (error "logic") . flip V.elemIndex indexesByDur)
                        mayCurIndex
                  in if newVecIdx < 0
                      then
                        Nothing -- no reverb at all can be considered a zero-length reverb
                      else
                        Just $ fromMaybe (error "logic") $ (V.!?) indexesByDur $ if newVecIdx >= nullIndex
                          then
                            nullIndex - 1
                          else
                            newVecIdx
        putIGame g { reverbs = rvbs { curRev = mayNewIndex} }
        maybe
          (liftIO (useReverb Nothing) >>= either (const $ liftIO $ putStrLn "error while unsetting reverb (see logs above)") return)
          (\newIdx -> when (mayCurIndex /= Just newIdx) $ do
              let (revPath, _) = Map.elemAt newIdx revs
                  (a,b) = splitFileName $ cpref ++ revPath
              liftIO $
                useReverb (Just (a,b)) >>= either
                  (const $ liftIO $ putStrLn "error while setting reverb (see logs above)")
                  return)
          mayNewIndex
      ChangeReverbWet i -> do
        let newWet = changeParam predefinedWetRatios wet i
        putIGame g { reverbs = rvbs { wetRatio = newWet } }
        liftIO (setReverbWetRatio $ realToFrac newWet) >>= either (const $ liftIO $ putStrLn "error while setting reverb wet ratio (see logs above)") return
      ToggleEnvelopeViewMode -> putIGame g {
        envelopePlot = EnvelopePlot (fromMaybe (error "logic") mayNewEnvMinMaxs) $ toggleView $ envViewMode $ envelopePlot g }
      ToggleEditMode ->
        putIGame g {edition = toggleEditMode $ edition g}
      ChangeEditedFeature i ->
        putIGame g {edition = setEditionIndex i $ edition g}
      InsertPressedKey k n ->
        putIGame g {clientPressedKeys = Map.insert k n pressed}
      RemovePressedKey k ->
        putIGame g {clientPressedKeys = Map.delete k pressed}


  onServerEvent e =
    -- TODO force withAnim when using putIGame ?
    getIGame >>= maybe (liftIO initialGame) return >>= \g -> withAnim $ case e of
      AssignedSourceIdx s -> do
        putIGame g { midiSourceIdx = Just s }
        asks writeToClient' >>= \f -> liftIO $ do
          FromClient . AppEvent . ChangeInstrument . withMinimumHarmonicsCount <$> loadInstrument >>= f
      NewLine seqId loopId ->
        putIGame $ g { pianoLoops = Map.insertWith Map.union seqId (Map.singleton loopId mkEmptyPressedKeys) $ pianoLoops g }
      PianoValue creator x -> putIGame $ either
        (\i ->
          g {pianos = Map.insert i x $ pianos g})
        (\(seqId,loopId) ->
          g {pianoLoops = Map.insertWith Map.union seqId (Map.singleton loopId x) $ pianoLoops g})
        creator

instance GameDraw SynthsGame where

  drawBackground (Screen _ center@(Coords _ centerC)) g@(SynthsGame pianoClients pianoLoops_ _ _ (EnvelopePlot curves _) _ edit _ _) = do
    drawInstructions Horizontally (Just $ widthEditMode edit) g (mkCentered $ move 21 Up center) >>= \(Alignment _ ref) -> do
      ref2 <- case curves of
        [] -> return ref
        [ahds,r] -> do
          let coordsEnv = move 45 LEFT $ move 1 Down ref
              heightPart = 20
              szAHDS = Size heightPart $ fromIntegral $ widthPart ahds
              szR = Size heightPart $ fromIntegral $ widthPart r
          drawEnv 0 ahds coordsEnv                        szAHDS (rgb 3 2 1)
          drawEnv 2 r    (move (widthPart ahds) RIGHT coordsEnv) szR    (rgb 2 3 1)
          return $ move (fromIntegral heightPart + 7) Down ref
        _ -> error "logic"
      ref3 <- showPianos
        "Players"
        showPlayerName
        pianoClients >>= drawPiano (move 1 Down ref2)
      foldM_
        (\r ((SequencerId seqId), seqPianoLoops) -> do
          showPianos
            ("Sequence " <> pack (show seqId))
            (\(LoopId creator idx) -> flip (<>) (" " <> CS.colored (pack (show idx)) (rgb 2 2 2)) <$> showPlayerName creator)
            seqPianoLoops >>= drawPiano r)
          ref3
          $ Map.assocs pianoLoops_
      return center

   where

    drawPiano (Coords r _) allStrs = do
      let maxL = fromMaybe 0 $ maximumMaybe $ map CS.countChars allStrs
          right = move (quot maxL 2) RIGHT $ Coords r centerC
      (Alignment _ res) <- foldM
        (\a s -> drawAligned s a)
        (mkRightAlign right)
        allStrs
      return res


    drawEnv offsetLegend (EnvelopePart resampled _) ul (Size h' _) fgColor = do
      let h = fromIntegral h'
          ll = move h Down ul
          color = onBlack fgColor
          heights x
            | countSamples x == 0 = []
            | otherwise = [round (minSample x * fromIntegral h)..round (maxSample x * fromIntegral h)]
      mapM_
        (\(i,mm) ->
          mapM_
            (\j -> drawGlyph (textGlyph '+') (translate ll $ Coords (-j) i) color)
            $ heights mm)
        $ zip [0..] resampled
      foldM_
        (\(cur,pos) n -> do
          let (q,r) = quotRem pos 4
              he
                | mod q 2 == 0 = 2
                | otherwise = 3
          when (r == 0) $ drawAt (CS.colored (pack $ show cur) fgColor) (move pos RIGHT $ move (offsetLegend + he) Down ll)
          return (cur + n,pos+1))
        (0,0)
        $ map countSamples resampled
        {-
      drawAligned_
        (CS.colored (pack $ show nSamples) color)
        $ mkRightAlign $ move (fromIntegral w - 1) RIGHT $ move 3 Down ll
-}
instance ServerInit SynthsServer where

  type ClientViewT SynthsServer = SynthsClientView

  mkInitialState _ =
    return (()
          , SynthsServer
              mempty
              (SequencerId 10) $ -- the 9 first sequencers are reserved
              srvMidiSource + 1)

  mkInitialClient = SynthsClientView mkEmptyPressedKeys mkEmptyRecording 0

instance ServerInParallel SynthsServer


instance Server SynthsServer where

  type ServerEventT SynthsServer = SynthsServerEvent

  greetNewcomer =
    (:[]) . AssignedSourceIdx <$>
      state
        (\s -> case unServerState s of
          server@(SynthsServer _ _ i) -> (i, s{ unServerState = server{nextMIDISourceIdx = mkMidiSourceIdx $ succ $ fromIntegral $ unMidiSourceIdx i}}))
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
            (\(loopId,(MusicLoop _ v)) -> do
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
                (\((Sequencer start _ _), (MusicLoop mus pianoV), progress) -> do
                  notifyEveryone $ NewLine seqId loopId
                  return
                    [ (\v -> playOnceFrom
                          (\m -> do
                            (nChanged',newPiano') <- liftIO $ modifyMVar pianoV $ \piano ->
                              let (nChanged,newPiano) = onMusic m piano
                              in return $ (newPiano, (nChanged,newPiano))
                            when (nChanged' > 0) $
                              modifyMVar_ v $ execStateT $ playLoopMusic seqId loopId newPiano' m)
                          start progress mus)
                    ]))

   where

    onRecordableNote :: (MonadIO m, MonadState (ServerState SynthsServer) m, MonadReader ConstClientView m)
                       => MusicalEvent InstrumentId
                       -> m [ServerEvent SynthsServer]
    onRecordableNote n = do
      cid <- asks clientId
      fmap (_piano . unClientView) . Map.lookup cid <$> gets clientsMap >>= maybe
        (return []) -- should never happen
        (\piano -> do
          let (countChangedNotes, piano') = onMusic n piano
          if countChangedNotes == 0
            then
              return []
            else do
              t <- liftIO getSystemTime
              creator <- asks clientId
              newpiano <- _piano <$> adjustClient (\s@(SynthsClientView _ recording _) ->
                    s { _piano = piano'
                      , _recording = recordMusic (ATM n t) recording })
              return
                [ ServerAppEvt $ PianoValue (Left creator) newpiano
                , PlayMusic n
                ])

    playLoopMusic :: (MonadState (ServerState SynthsServer) m, MonadIO m)
                  => SequencerId -> LoopId -> PressedKeys InstrumentId -> MusicalEvent InstrumentId -> m ()
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
          seqId <- modifyState' $ \(SynthsServer m i n) ->
            let (sid,succI) = maybe (i,succ i) (\j -> (j,i)) maySeqId
            in (sid,SynthsServer (Map.insert sid sequencer m) succI n)
          notifyEveryone $ NewLine seqId loopId
          return
            [ (\v -> forever $ do
                now' <- getSystemTime
                modifyMVar sequencer (\(Sequencer _ a b) -> do
                  let s = (Sequencer now' a b)
                  return (s,s))
                  >>= \(Sequencer startTime duration vecs) -> do
                    forM_ (Map.assocs vecs) $ \(lid,(MusicLoop vec pianoV)) ->
                      void $ forkIO $
                        playOnce
                          (\m -> do
                            (nChanged',newPiano') <- modifyMVar pianoV $ \piano ->
                              let (nChanged,newPiano) = onMusic m piano
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
          concat <$> forM (releaseAllKeys piano) onRecordableNote >>= notifyEveryoneN'
          fmap (_recording . unClientView) . Map.lookup cid <$> gets clientsMap >>= maybe
            (return [])
            (\recording -> do
              creator <- asks clientId
              (SynthsClientView _ _ idx) <- adjustClient
                (\(SynthsClientView _ _ loopIdx) ->
                  SynthsClientView mkEmptyPressedKeys mkEmptyRecording $ loopIdx + 1)
              let loopId = LoopId creator $ idx - 1
              liftIO getSystemTime >>= x loopId recording))

pianoEvts :: Either ClientId (SequencerId,LoopId) -> PressedKeys InstrumentId -> [ServerEvent SynthsServer]
pianoEvts idx v@(PressedKeys m) =
  ServerAppEvt (PianoValue idx v) :
  concatMap
    (\(note,n) ->
    -- TODO the server could be the "0" midisource, and generate its own midi timestamps
      replicate n $ PlayMusic (StartNote Nothing note 1))
    (Map.assocs m)

instance ServerCmdParser SynthsServer


{-# INLINABLE showPianos #-}
showPianos :: MonadReader e m
           => Text
           -> (a -> m ColorString)
           -> Map a (PressedKeys InstrumentId)
           -> m [ColorString]
showPianos _ _ Tip = return []
showPianos title showKey m = do
  let minNote = noteToMidiPitch Do $ noOctave - 1
      maxNote = noteToMidiPitch Sol $ noOctave + 1
  showArray (Just (CS.colored title $ rgb 2 1 2,"")) <$> mapM
    (\(i,piano) -> flip (,) (CS.colored (pack $ showKeys minNote maxNote piano) $ rgb 3 1 2) <$> showKey i)
    (Map.assocs m)

showKeys :: MidiPitch
         -- ^ From
         -> MidiPitch
         -- ^ To
         -> PressedKeys InstrumentId
         -> String
showKeys from to (PressedKeys m) =
  go [] [from..to] $ Set.toAscList $ Map.keysSet m
 where
  go l [] _ = reverse l
  go l (k:ks) remainingPressed =
    case remainingPressed of
      [] -> go' freeChar remainingPressed
      (InstrumentNote pressedNoteName pressedNoteOctave _):ps ->
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
      | whiteKeyPitch k = '-'
      | otherwise = '*'

    pressedChar
      | whiteKeyPitch k = '_'
      | otherwise = '.'
