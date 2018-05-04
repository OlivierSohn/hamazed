{-# OPTIONS_HADDOCK hide #-}

{-# LANGUAGE NoImplicitPrelude #-}

-- | This module handles time constants of the game logic.

module Imj.Game.Hamazed.Timing
        ( gameMotionPeriod
        , GameTime
        , initalGameMultiplicator
        ) where

import           Imj.Prelude

import           Imj.Timing

{-# INLINE gameMotionPeriod #-}
-- | Nominal game period when the associated 'Multiplicator' is 1.
gameMotionPeriod :: Time Duration GameTime
gameMotionPeriod = fromSecs 0.16

data GameTime

initalGameMultiplicator :: Multiplicator GameTime
initalGameMultiplicator = gameTimeMultiplicator 1

gameTimeMultiplicator :: Double -> Multiplicator GameTime
gameTimeMultiplicator = Multiplicator
