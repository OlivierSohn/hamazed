module Main where

import           Control.Monad(void)

import           Imj.Control.Concurrent
import           Imj.Game.Hamazed.Run( run )

main :: IO ()
main = do
  useOneCapabilityPerPhysicalCore
  void run
