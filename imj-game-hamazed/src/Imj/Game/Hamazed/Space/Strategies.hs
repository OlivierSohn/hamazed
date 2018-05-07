{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Imj.Game.Hamazed.Space.Strategies
    ( optimalStrategies
    ) where

import           Imj.Game.Hamazed.Space.Strategies.Internal
import           Imj.Space.Strategies

optimalStrategies :: OptimalStrategies
optimalStrategies = $(readOptimalStrategies optimalStrategiesFilepath)
