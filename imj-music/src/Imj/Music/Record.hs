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
import           Imj.Timing

recordMusic :: Time Point System -> Recording -> Music -> Recording
recordMusic t (Recording r) m = Recording (flip (:) r $ ATM m t)

mkSequencerFromRecording :: k -> Recording -> Time Point System -> IO (Either Text (Sequencer k))
mkSequencerFromRecording _ (Recording []) _ = return $ Left "empty recording"
mkSequencerFromRecording k (Recording r) start = do
  mus <- mkMusicLine v
  return $ Right $ Sequencer start (firstTime...start) $ Map.singleton k mus
 where
  rr = reverse r
  (ATM _ firstTime) = fromMaybe (error "logic") $ listToMaybe rr
  v = V.fromList $ map (\(ATM m t) -> RTM m $ firstTime...t) rr

{-# INLINABLE insertRecording #-}
insertRecording :: Ord k
                => Recording -> k -> Sequencer k -> IO (Either Text (Sequencer k, MusicLine))
insertRecording (Recording []) _ _ = return $ Left "Recording is empty"
insertRecording (Recording r@(_:_)) k (Sequencer curPeriodStart periodLength ls)
  | periodLength == zeroDuration = return $ Left "sequencer with zero duration"
  | otherwise = do
      mus <- mkMusicLine v
      return $ Right $
        (Sequencer curPeriodStart periodLength $ Map.insert k mus ls
       , mus)
 where
  rr = reverse r
  (ATM _ firstTime) = fromMaybe (error "logic") $ listToMaybe rr
  recordingTimeShift = (fromIntegral nPeriodsShift) .* periodLength
  nPeriodsShift = floor $ (curPeriodStart...firstTime) ./ periodLength :: Int
  refTime = addDuration recordingTimeShift curPeriodStart
  v = V.fromList $ map (\(ATM m t) -> RTM m $ refTime...t) rr

startLoopThreadAt :: MonadIO m
                  => (Music -> m ())
                  -> Time Point System
                  -> Time Duration System
                  -> V.Vector RelativeTimedMusic
                  -> m ()
startLoopThreadAt play begin elapsed l =
  playOnce play v begin
 where
  v = V.dropWhile (\(RTM _ dt) -> dt < elapsed) l

playOnce :: MonadIO m
         => (Music -> m ())
         -> V.Vector RelativeTimedMusic
         -> Time Point System
         -- ^ The reference time
         -> m ()
playOnce play v begin =

  go 0

 where

  !len = V.length v

  go index
    | index == len = return ()
    | otherwise = do
      let (RTM m dt) = V.unsafeIndex v index
          nextEventTime = addDuration dt begin
      now <- liftIO getSystemTime
      let waitDuration = now...nextEventTime
      liftIO $ threadDelay $ fromIntegral $ toMicros waitDuration
      play m
      go $ index + 1
