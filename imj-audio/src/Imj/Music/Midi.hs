{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.Music.Midi
      ( -- * Easy
        playMidiFile
      -- * Advanced
      , mkMidiPartition
      , runMidiPartitionWith
      , midiEventHandler'PlayAnd
      , PlayState(..)
      , ActiveNotes
      -- * Reexported for tests
      , hasFinishedConsistently
      ) where

import           Prelude(print, putStrLn, fromRational)
import           Imj.Prelude

import           Control.Concurrent(threadDelay)
import           Data.EventList.Relative.TimeBody (toPairList)
import           Data.List(replicate, length, deleteBy, sort, sortOn, foldl')
import           Data.Map.Strict(Map, alter, insertWith)
import qualified Data.Map.Strict as Map
import           Numeric.NonNegative.Wrapper(toNumber, Rational)
import qualified Sound.MIDI.File.Event as Evt
import           Sound.MIDI.File.Load
import           Sound.MIDI.File
import qualified Sound.MIDI.Message.Channel       as ChannelMsg
import qualified Sound.MIDI.Message.Channel.Voice as Voice

import           Imj.Audio
import           Imj.Timing

-- TODO use intermediate object : Vector Unboxed Pair ...
-- TODO provide a volume parameter to avoid saturation, and a function to map 0..127 velocity to 0..1.
-- TODO provide the choice of volume adjustment wrt frequency (yes, no, continuous)
-- TODO use white noise to handle the drums on beginning of /Users/Olivier/Dev/hs.hamazed/liszt_hungarian_fantasia_for_orchestra_(c)laviano.mid

-- | Plays a midifile, and prints unhandled events to stdout.
--
-- Must be called within an action passed to 'usingAudioOutput' or 'usingAudioOutputWithMinLatency'
playMidiFile :: FilePath
             -> Instrument
             -> IO (Either ActiveNotes ActiveNotes)
playMidiFile p i =
  mkMidiPartition p >>= runMidiPartitionWith
    (midiEventHandler'PlayAnd i print mkEmptyActiveNotes insertNote removeNote maybeRender)

data ActiveNotes = AN {
    _byChannel :: Map ChannelMsg.Channel ChannelState
  , _step :: !Step
  , _renderedAt :: !Step
} deriving(Show, Eq)

mkEmptyActiveNotes :: ActiveNotes
mkEmptyActiveNotes = AN mempty firstStep (pred firstStep)

hasFinishedConsistently :: ActiveNotes -> Either Text ()
hasFinishedConsistently (AN ma a b)
  | countActiveNotes > 0 = Left "didn't finish playing"
  | a /= b = Left "did't render all"
  | otherwise = Right ()
 where
  countActiveNotes = Map.foldl' (\n (CS _ l) -> n + length l) 0 ma

data ChannelState = CS {
    _order :: !Int
    -- ^ order of appearance of the channel in the song.
  , _activeNotes :: !ChannelNotes
} deriving(Show, Eq)

type ChannelNotes = [(Voice.Pitch, Voice.Velocity, Rank, Step)]
    -- ^ The 'Rank's are unique among the list, strictly positive,
    -- and inserted elements are assigned a minimal rank.

newtype Rank = Rank Int
  deriving(Show, Ord, Eq, Enum, Integral, Real, Num)

minRank :: Rank
minRank = Rank 0

newtype Step = Step Int
  deriving(Show, Enum, Eq, Ord)

firstStep :: Step
firstStep = Step 0

data PlayState a = PlayState {
    initialState :: !a
  , onMidiEvent :: Evt.T -> a -> IO (a, Either () ())
  -- ^ Is called when 'runMidiPartitionWith' encounters a midi event.
  --  Returns 'True' to continue playing.
  , onMayRender :: a -> IO a
    -- ^ Is called when 'runMidiPartitionWith' detects a gap between the last handled event
    -- and the next one.
}

-- | This handler plays the midi notes, and lets you customize the rendering
-- by passing appropriate parameters.
midiEventHandler'PlayAnd :: Instrument
                         -> (Evt.T -> IO ())
                         -- ^ Called when a non-note event is encountered.
                         -> a
                         -- ^ Initial state
                         -> (ChannelMsg.Channel -> Voice.Pitch -> Voice.Velocity -> a -> a)
                         -- ^ Called to update the state when a note has started playing
                         -> (ChannelMsg.Channel -> Voice.Pitch -> a -> a)
                         -- ^ Called to update the state when a note has stopped playing
                         -> (a -> IO a)
                         -- ^ Called when it is time to maybe-render 'a'
                         -> PlayState a
midiEventHandler'PlayAnd instrument onOtherEvent i onStart onStop mayRender =
  PlayState i onMidiEvt mayRender
 where
  onMidiEvt evt s = case evt of
    (Evt.MIDIEvent (ChannelMsg.Cons channel body)) -> case body of
      ChannelMsg.Voice v -> case v of
        Voice.NoteOn pitch vel ->
          (,)
            (onStart channel pitch vel s)
            <$> play (StartNote Nothing
                  (mkInstrumentNote (fromIntegral $ Voice.fromPitch pitch) instrument)
                  $ mkNoteVelocity $ Voice.fromVelocity vel)
        Voice.NoteOff pitch _ ->
          (,)
            (onStop channel pitch s)
            <$> play (StopNote Nothing $ mkInstrumentNote (fromIntegral $ Voice.fromPitch pitch) instrument)
        _ -> do
          onOtherEvent evt
          return (s,Right ())
      _ -> do
        onOtherEvent evt
        return (s,Right ())
    _ -> do
      onOtherEvent evt
      return (s,Right ())

prettyShowChannels :: ActiveNotes -> String
prettyShowChannels (AN an _ prevRenderedI) =
  concatMap
    (justifyL 20
      . fst
      . foldl'
          (\(str,prevNoteRank) (pitch,_,noteRank,noteStep) ->
            (str
            ++
            replicate
              -- 2 is assuming that pitch is <= 99
              (bool 1 0 (null str) + fromIntegral ((*) (2+1) $ noteRank - prevNoteRank - 1))
              ' '
            ++
            -- 2 is assuming that pitch is <= 99
            justifyR 2
              (if (noteStep > prevRenderedI)
                then
                  show $ Voice.fromPitch pitch
                else
                  "|")
            , noteRank))
          ("",pred minRank)
      . sortOn (\(_,_,r,_) -> r))
    $ map (\(CS _ l) -> l)
    $ sortOn _order
    $ Map.elems an

maybeRender :: ActiveNotes -> IO ActiveNotes
maybeRender initial@(AN a b c)
  | b == c = return initial -- this version has already been drawn
  | b < c = error "logic"
  | otherwise = do
      putStrLn $ prettyShowChannels initial
      return $ AN a b b

availableRank :: ChannelNotes -> Rank
availableRank = go minRank . sort . map (\(_,_,r,_)->r)
 where
  go candidate [] = candidate
  go candidate (r:rest)
    | candidate == r = go (succ candidate) rest
    | candidate > r = error $ "logic " ++ show (r, candidate)
    | otherwise = candidate

insertNote :: ChannelMsg.Channel -> Voice.Pitch -> Voice.Velocity -> ActiveNotes -> ActiveNotes
insertNote chan pitch vel (AN an i r) =
  AN an' (succ i) r
 where
  an' = insertWith
    (\(CS _ newNotes) (CS order curNotes) -> case newNotes of
        [_] -> CS order $ (pitch, vel, availableRank curNotes, succ i): curNotes
        _ -> error "logic")
    chan
    (CS (Map.size an) [(pitch, vel, minRank, succ i)])
    an

removeNote :: ChannelMsg.Channel -> Voice.Pitch -> ActiveNotes -> ActiveNotes
removeNote chan pitch (AN an i r) =
  AN an' (succ i) r
 where
  an' = alter
    (maybe
      (error "logic")
      (\(CS order curNotes) -> Just $ CS order $ deleteBy (\_ (p, _, _, _) -> p == pitch) undefined curNotes
        ))
    chan
    an

mkMidiPartition :: FilePath
                -> IO [(Rational, Evt.T)]
mkMidiPartition p = do
  (Cons midiType division tracks) <- explicitNoteOff <$> fromFile p
  return $ toPairList $ secondsFromTicks division $ mergeTracks midiType tracks


runMidiPartitionWith :: PlayState a
                     -- ^ 'PlayState' encapsulates the logic to render
                     -- (play, print, etc...) the played notes.
                     -> [(Rational, Evt.T)]
                     -> IO (Either a a)
runMidiPartitionWith stateFuns part = do
  start <- getSystemTime
  run part start
 where
  run a start =
    go a 0 $ initialState stateFuns
   where
    go [] _ state1 = do
      Right <$> onMayRender stateFuns state1
    go ((timeToWait,whatToPlay):rest) time state1 = do
      let wait = toNumber timeToWait
      state2 <-
        if wait > 0
          then
            -- we may wait so we allow to render.
            onMayRender stateFuns state1
          else
            return state1
      let newTime = time + wait
          newTimeApprox = addDuration (fromSecs $ fromRational newTime) start
      now <- getSystemTime
      let duration = fromIntegral $ toMicros $ now...newTimeApprox
      when (duration > 0) $ threadDelay duration
      (state3, continue) <- onMidiEvent stateFuns whatToPlay state2
      either
        (const $ return $ Left state3)
        (const $ go rest newTime state3)
        continue


justifyR, justifyL :: Int -> String -> String
justifyR n x =
  replicate (n-length x) ' ' <> x
justifyL n x =
  x <> replicate (n-length x) ' '
