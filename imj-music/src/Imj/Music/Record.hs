{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Imj.Music.Record
      ( recordMusic
      , mkSequencerFromRecording
      , playOnce
      , startLoopThreadAt
      , insertRecording
      ) where

import           Imj.Prelude
import           Control.Concurrent(threadDelay)
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V

import           Imj.Music.Types
import           Imj.Music.Piano
import           Imj.Timing

recordMusic :: Time Point System -> Recording -> Music -> Instrument -> Recording
recordMusic t (Recording r) m i = Recording (flip (:) r $ ATM m i t)

mkSequencerFromRecording :: k -> Recording -> Time Point System -> Either Text (Sequencer k)
mkSequencerFromRecording _ (Recording []) _ = Left "empty recording"
mkSequencerFromRecording k (Recording r) start =
  Right $ Sequencer start (firstTime...start) $ Map.singleton k v
 where
  rr = reverse r
  (ATM _ _ firstTime) = fromMaybe (error "logic") $ listToMaybe rr
  v = V.fromList $ map (\(ATM m i t) -> RTM m i $ firstTime...t) rr

{-# INLINABLE insertRecording #-}
insertRecording :: Ord k
                => Recording -> k -> Sequencer k -> Either Text (Sequencer k, V.Vector RelativeTimedMusic)
insertRecording (Recording []) _ _ = Left "Recording is empty"
insertRecording (Recording r@(_:_)) k (Sequencer curPeriodStart periodLength ls)
  | periodLength == zeroDuration = Left "sequencer with zero duration"
  | otherwise = Right $
      (Sequencer curPeriodStart periodLength $ Map.insert k v ls
     , v)
 where
  rr = reverse r
  (ATM _ _ firstTime) = fromMaybe (error "logic") $ listToMaybe rr
  recordingTimeShift = (fromIntegral nPeriodsShift) .* periodLength
  nPeriodsShift = floor $ (curPeriodStart...firstTime) ./ periodLength :: Int
  refTime = addDuration recordingTimeShift curPeriodStart
  v = V.fromList $ map (\(ATM m i t) -> RTM m i $ refTime...t) rr

startLoopThreadAt :: MonadIO m
                  => (PianoState -> Music -> Instrument -> m ())
                  -> Time Point System
                  -> Time Duration System
                  -> V.Vector RelativeTimedMusic
                  -> m ()
startLoopThreadAt play begin elapsed l =
  playOnce play v begin
 where
  v = V.dropWhile (\(RTM _ _ dt) -> dt < elapsed) l

playOnce :: MonadIO m
         => (PianoState -> Music -> Instrument -> m ())
         -> V.Vector RelativeTimedMusic
         -> Time Point System
         -- ^ The reference time
         -> m ()
playOnce play v begin =

  go mkEmptyPiano 0

 where

  !len = V.length v

  go piano index
    | index == len = return ()
    | otherwise = do
      let (RTM m i dt) = V.unsafeIndex v index
          nextEventTime = addDuration dt begin
      now <- liftIO getSystemTime
      let waitDuration = now...nextEventTime
      liftIO $ threadDelay $ fromIntegral $ toMicros waitDuration
      let (nChanged,newPiano) = modPiano m piano
      when (nChanged > 0) $ play newPiano m i
      go newPiano $ index + 1
