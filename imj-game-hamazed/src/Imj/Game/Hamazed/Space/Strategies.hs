{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}

module Imj.Game.Hamazed.Space.Strategies
    ( optimalStrategies
    ) where

import           Data.FileEmbed(embedFile)
import qualified Data.ByteString as B

import           Imj.Game.Hamazed.Space.Strategies.Internal

optimalStrategies :: B.ByteString
optimalStrategies = $(embedFile optimalStrategiesFilepath) -- TODO also convert to OptimalStrategies at build time
