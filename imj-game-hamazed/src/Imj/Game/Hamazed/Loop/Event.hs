{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Game.Hamazed.Loop.Event
    ( isPrincipal
    , mkEmptyGroup
    , visible
    , count
    , tryGrow
    -- * Reexports
    , module Imj.Game.Hamazed.Loop.Event.Types
    ) where

import           Imj.Prelude
import           Prelude(length)

import           Imj.Game.Hamazed.Types
import           Imj.Game.Hamazed.Loop.Event.Types
import           Imj.Game.Hamazed.Loop.Timing
import           Imj.Graphics.ParticleSystem.Design.Timing

-- | No 2 principal events can be part of the same 'EventGroup'.
-- It allows to separate important game action on different rendered frames.
isPrincipal :: UpdateEvent -> Bool
isPrincipal (Right e) = case e of
  (Timeout (Deadline _ _ (AnimateParticleSystem _))) -> False
  (Timeout (Deadline _ _ DisplayContinueMessage)) -> False
  (Timeout (Deadline _ _ AnimateUI)) -> False
  _ -> True
isPrincipal (Left _) = True

mkEmptyGroup :: EventGroup
mkEmptyGroup = EventGroup [] False zeroDuration Nothing

visible :: EventGroup -> Bool
visible (EventGroup _ _ _ Nothing) = False
visible _ = True

count :: EventGroup -> Int
count (EventGroup l _ _ _) = length l

tryGrow :: Maybe UpdateEvent -> EventGroup -> IO (Maybe EventGroup)
tryGrow Nothing group
 | null $ events group = return $ Just group -- Keep the group opened to NOT do a render
 | otherwise = return Nothing -- to do a render
tryGrow (Just e) (EventGroup l hasPrincipal updateTime range)
 | hasPrincipal && principal = return Nothing -- we don't allow two principal events in the same group
 | updateTime > fromSecs 0.01 = return Nothing -- we limit the duration of updates, to keep a stable render rate
 | otherwise = maybe mkRangeSingleton (flip extendRange) range <$> time >>= \range' -> do
    let -- so that no 2 updates of the same particle system are done in the same group:
        maxDiameter = particleSystemDurationToSystemDuration $ 0.99 .* particleSystemPeriod
    if timeSpan range' > maxDiameter
      then
        --putStrLn (show (diam, maxDiameter)) >>
        return Nothing
      else
        return $ withEvent $ Just range'
 where
  !principal = isPrincipal e
  withEvent = Just . EventGroup (e:l) (hasPrincipal || principal) updateTime
  time = case e of
    Right (Timeout (Deadline t _ _)) -> return t
    _ -> getSystemTime
