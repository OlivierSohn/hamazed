{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}

module Imj.Game.Hamazed.Loop.Event
    ( isVisible
    , isPrincipal
    , EventGroup(..)
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
import           Imj.Graphics.ParticleSystem.Design.Timing
import           Imj.Timing
import           Imj.Util hiding(range)

-- | Tells if after handling an 'Event' we should render or not.
isVisible :: Event -> Bool
isVisible = \case
  (Action Ship _) -> False -- When the ship accelerates, nothing changes visually
  _ -> True

-- | No 2 principal events can be part of the same 'EventGroup'.
-- It allows to separate important game action on different rendered frames.
isPrincipal :: Event -> Bool
isPrincipal = \case
  (Timeout (Deadline _ _ AnimateParticleSystems)) -> False
  (Timeout (Deadline _ _ DisplayContinueMessage)) -> False
  (Timeout (Deadline _ _ AnimateUI)) -> False
  _ -> True

data EventGroup = EventGroup {
    _eventGroupList :: ![Event]
  , _eventGroupHasPrincipal :: !Bool
  , _eventGroupUpdateTime :: !TimeSpec
  , _eventGroupVisibleTimeRange :: !(Maybe (Range TimeSpec))
  -- ^ Range of /visible/ events deadlines
}

mkEmptyGroup :: EventGroup
mkEmptyGroup = EventGroup [] False zeroTime Nothing

visible :: EventGroup -> Bool
visible (EventGroup _ _ _ Nothing) = False
visible _ = True

count :: EventGroup -> Int
count (EventGroup l _ _ _) = length l

tryGrow :: Maybe Event -> EventGroup -> IO (Maybe EventGroup)
-- when the group is not empty, a Nothing will close it, to do a render:
tryGrow Nothing (EventGroup (_:_) _ _ _) = return Nothing
-- when the group is empty, a Nothing will keep it opened, to avoid doing a useless render:
tryGrow Nothing emptyGroup = return $ Just emptyGroup
tryGrow (Just e) (EventGroup l hasPrincipal updateTime range)
-- don't allow two principal events in the same group:
 |hasPrincipal && principal = return Nothing
-- limit the duration of updates, to keep a stable render rate:
 |updateTime > secondsToTimeSpec 0.01 = return Nothing
 |isVisible e = maybe mkRangeSingleton (flip extendRange) range <$> time >>= \range' -> do
    let -- so that no 2 updates of the same particle system are done in the same group:
        maxDiameter = secondsToTimeSpec (particleSystemPeriod * 0.99)
        diam = diameter range'
    if diam > maxDiameter
      then
        --putStrLn (show (diam, maxDiameter)) >>
        return Nothing
      else
        return $ withEvent $ Just range'
 |otherwise = return $ withEvent range
 where
  !principal = isPrincipal e
  withEvent = Just . EventGroup (e:l) (hasPrincipal || principal) updateTime
  time = case e of
    (Timeout (Deadline (KeyTime t) _ _)) -> return t
    _ -> getSystemTime
