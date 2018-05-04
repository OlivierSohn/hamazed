module Main where

import           Control.Monad(void)
import           Data.Proxy(Proxy(..))

import           Imj.Control.Concurrent(useOneCapabilityPerPhysicalCore)

import           Imj.Game.App(runGame)
import           Imj.Game.Hamazed(HamazedGame)

main :: IO ()
main = do
  useOneCapabilityPerPhysicalCore -- this is important to optimize world creation time.
  void $ runGame (Proxy :: Proxy HamazedGame)
