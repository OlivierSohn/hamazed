
-- | This modules handle time constants of the game logic.

module Imj.Game.Timing
        ( addGameStepDuration
        , module Imj.Timing
        ) where

import           Imj.Timing

gamePeriod :: DiffTime
gamePeriod = fromIntegral gamePeriodMicros / 1000000

-- using the "incremental" render backend, there is no flicker
-- using the "full" render backend, flicker starts at 40
gamePeriodMicros :: Int
gamePeriodMicros = gamePeriodMillis * 1000
  where
    gamePeriodMillis = 160 -- this controls the game loop frequency.
                           -- 20 seems to match screen refresh frequency

addGameStepDuration :: KeyTime -> KeyTime
addGameStepDuration = addDuration gamePeriod
