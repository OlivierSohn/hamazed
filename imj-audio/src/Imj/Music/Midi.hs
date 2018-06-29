{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Imj.Music.Midi
      ( playMidiFile
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
playMidiFile :: FilePath
             -> IO (Either () ())
playMidiFile p =
  mkMidiPartition p >>= playMidiPartition

data ChannelState = CS {
    _order :: !Int
    -- ^ order of appearance of the channel in the song.
  , _activeNotes :: !ChannelNotes
}
data ActiveNotes = AN {
    _byChannel :: Map ChannelMsg.Channel ChannelState
  , _step :: !Step
  , _renderedAt :: !Step
}

mkEmptyActiveNotes :: ActiveNotes
mkEmptyActiveNotes = AN mempty firstStep (pred firstStep)

newtype Rank = Rank Int
  deriving(Show, Ord, Eq, Enum, Integral, Real, Num)

minRank :: Rank
minRank = Rank 0

newtype Step = Step Int
  deriving(Show, Enum, Eq, Ord)

firstStep :: Step
firstStep = Step 0

type ChannelNotes = [(Voice.Pitch, Voice.Velocity, Rank, Step)]
    -- ^ The 'Rank's are unique among the list, strictly positive,
    -- and inserted elements are assigned a minimal rank.

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

playMidiPartition :: [(Rational, Evt.T)]
                  -> IO (Either () ())
playMidiPartition part = do
  start <- getSystemTime
  run part start
 where
  run a start =
    go a 0 mkEmptyActiveNotes
   where
    go [] _ _ = return $ Right ()
    go ((timeToWait,whatToPlay):rest) time initialActiveNotes = do
      let mayAct = case whatToPlay of
            (Evt.MIDIEvent (ChannelMsg.Cons channel body)) -> case body of
              ChannelMsg.Mode _ -> Just (print whatToPlay >> return True, id)
              ChannelMsg.Voice v -> case v of
                Voice.NoteOn pitch vel ->
                  Just (do
                    play $ StartNote
                      (mkInstrumentNote (fromIntegral $ Voice.fromPitch pitch) simpleInstrument)
                      $ mkNoteVelocity $ Voice.fromVelocity vel
                  , insertNote channel pitch vel)
                Voice.NoteOff pitch _ ->
                  Just
                    (play $ StopNote $ mkInstrumentNote (fromIntegral $ Voice.fromPitch pitch) simpleInstrument
                  , removeNote channel pitch)
                _ -> Nothing
            _ -> Just (print whatToPlay >> return True, id)
      let wait = toNumber timeToWait
          newTime = time + wait
      activeNotes <-
        if wait > 0
          then
            -- we may wait so we allow to render.
            maybeRender initialActiveNotes
          else
            return initialActiveNotes
      maybe
        (go rest newTime activeNotes)
        (\(action, stateNotes) -> do
            let newTimeApprox = addDuration (fromSecs $ fromRational newTime) start
            now <- getSystemTime
            let duration = toMicros $ now...newTimeApprox
                newActiveNotes = stateNotes activeNotes
            when (duration > 0) $ threadDelay $ fromIntegral duration
            continue <- action
            if continue
              then
                go rest newTime newActiveNotes
              else
                return $ Left ()
          )
        mayAct

justifyR, justifyL :: Int -> String -> String
justifyR n x =
  replicate (n-length x) ' ' <> x
justifyL n x =
  x <> replicate (n-length x) ' '
