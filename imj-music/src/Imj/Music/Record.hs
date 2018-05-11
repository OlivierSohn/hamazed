{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE BangPatterns #-}

module Imj.Music.Record
      ( recordMusic
      , mkLoop
      , playLoopOnce
      ) where

import           Imj.Prelude
import           Control.Concurrent(threadDelay)
import qualified Data.Vector as V

import           Imj.Music.Types
import           Imj.Timing

recordMusic :: Time Point System -> Recording -> Music -> Instrument -> Recording
recordMusic t (Recording r) m i = Recording $ flip (:) r $ ATM m i t

mkLoop :: Time Point System -> Recording -> Loop
mkLoop !endTime (Recording r) = Loop v $ Just minDuration
 where
  rr = reverse r
  (ATM _ _ firstTime) = fromMaybe (error "logic") $ listToMaybe rr
  minDuration = firstTime ... endTime
  v = V.fromList $ map (\(ATM m i t) -> RTM m i $ firstTime...t) rr

playLoopOnce :: MonadIO m
             => (Music -> Instrument -> m ())
             -> Loop
             -> m ()
playLoopOnce play (Loop v mayMinDuration) =

  liftIO getSystemTime >>= playL

 where

  !len = V.length v

  playL begin =

    go 0

   where

    go index
      | index == len = waitTillLoopEnd
      | otherwise = do
        let (RTM m i _) = V.unsafeIndex v index
        play m i
        if index == len - 1
          then
            waitTillLoopEnd
          else do
            now <- liftIO getSystemTime
            let (RTM _ _ dt) = V.unsafeIndex v $ index + 1
                nextEventTime = addDuration dt begin
                waitDuration = now...nextEventTime
            liftIO $ threadDelay $ fromIntegral $ toMicros waitDuration
            go $ index + 1

    waitTillLoopEnd = maybe
      (return ())
      (\minDuration -> do
        now <- liftIO getSystemTime
        let elapsed = begin...now
            remaining = fromIntegral $ toMicros $ minDuration |-| elapsed
        when (remaining > 0) $
          liftIO $ threadDelay remaining)
      mayMinDuration
